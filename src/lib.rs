// Copyright (C) 2024 Matthew Waters <matthew@centricular.com>
//
// Licensed under the MIT license <LICENSE-MIT> or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! # cea608-types
//!
//! Provides the necessary infrastructure to read and write CEA-608 byte pairs
//!
//! The reference for this implementation is the [ANSI/CTA-608-E S-2019](https://shop.cta.tech/products/line-21-data-services) specification.

use tables::{Channel, Code, MidRow, PreambleAddressCode};

#[macro_use]
extern crate log;

pub mod tables;

/// Various possible errors when parsing data
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum ParserError {
    /// Invalid parity
    #[error("Invalid parity")]
    InvalidParity,
    /// Length of data does not match length advertised
    #[error("Length of the data ({actual}) does not match the expected length ({expected})")]
    LengthMismatch {
        /// The expected size
        expected: usize,
        /// The actual size
        actual: usize,
    },
}

/// An error enum returned when writing data fails
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum WriterError {
    /// Writing would overflow by how many bytes
    #[error("Writing would overflow by {0} bytes")]
    WouldOverflow(usize),
    /// It is not possible to write to this resource
    #[error("Read only resource")]
    ReadOnly,
}

impl From<tables::CodeError> for ParserError {
    fn from(err: tables::CodeError) -> Self {
        match err {
            tables::CodeError::LengthMismatch { expected, actual } => {
                ParserError::LengthMismatch { expected, actual }
            }
            tables::CodeError::InvalidParity => ParserError::InvalidParity,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Mode {
    PopOn,
    PaintOn,
    RollUp2,
    RollUp3,
    RollUp4,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Text {
    pub char1: Option<char>,
    pub char2: Option<char>,
    pub channel: Channel,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Cea608 {
    Text(Text),
    NewMode(Channel, Mode),
    EraseDisplay(Channel),
    EraseNonDisplay(Channel),
    CarriageReturn(Channel),
    Backspace(Channel),
    EndOfCaption(Channel),
    TabOffset(Channel, u8),
    DeleteToEndOfRow(Channel),
    Preamble(Channel, PreambleAddressCode),
    MidRowChange(Channel, MidRow),
}

impl Cea608 {
    pub fn channel(&self) -> Channel {
        match self {
            Self::Text(text) => text.channel,
            Self::NewMode(chan, _) => *chan,
            Self::EraseDisplay(chan) => *chan,
            Self::EraseNonDisplay(chan) => *chan,
            Self::CarriageReturn(chan) => *chan,
            Self::Backspace(chan) => *chan,
            Self::EndOfCaption(chan) => *chan,
            Self::TabOffset(chan, _) => *chan,
            Self::Preamble(chan, _) => *chan,
            Self::MidRowChange(chan, _) => *chan,
            Self::DeleteToEndOfRow(chan) => *chan,
        }
    }
}

/// Helper struct that has two purposes:
/// 1. Tracks the previous data for control code de-duplication
/// 2. Adds the last received channel to non control codes.
#[derive(Debug, Default)]
pub struct Cea608State {
    last_data: Option<[u8; 2]>,
    last_channel: Option<Channel>,
}

impl Cea608State {
    pub fn decode(&mut self, data: [u8; 2]) -> Result<Option<Cea608>, ParserError> {
        trace!("decoding {data:x?}, last data {:x?}", self.last_data);
        let code = Code::from_data(data)?;

        if Some(data) == self.last_data {
            if let Code::Control(_control) = code[0] {
                debug!("Skipping duplicate");
                return Ok(None);
            }
        }
        self.last_data = Some(data);

        // TODO: handle xds and text mode

        match code {
            [Code::Control(control_code), _] => {
                let channel = control_code.channel();
                self.last_channel = Some(channel);
                Ok(Some(match control_code.code() {
                    tables::Control::MidRow(midrow) => Cea608::MidRowChange(channel, midrow),
                    tables::Control::PreambleAddress(preamble) => {
                        Cea608::Preamble(channel, preamble)
                    }
                    tables::Control::EraseDisplayedMemory => Cea608::EraseDisplay(channel),
                    tables::Control::EraseNonDisplayedMemory => Cea608::EraseNonDisplay(channel),
                    tables::Control::CarriageReturn => Cea608::CarriageReturn(channel),
                    tables::Control::Backspace => Cea608::Backspace(channel),
                    tables::Control::EndOfCaption => Cea608::EndOfCaption(channel),
                    tables::Control::RollUp2 => Cea608::NewMode(channel, Mode::RollUp2),
                    tables::Control::RollUp3 => Cea608::NewMode(channel, Mode::RollUp3),
                    tables::Control::RollUp4 => Cea608::NewMode(channel, Mode::RollUp4),
                    tables::Control::ResumeDirectionCaptioning => {
                        Cea608::NewMode(channel, Mode::PaintOn)
                    }
                    tables::Control::ResumeCaptionLoading => Cea608::NewMode(channel, Mode::PopOn),
                    tables::Control::TabOffset1 => Cea608::TabOffset(channel, 1),
                    tables::Control::TabOffset2 => Cea608::TabOffset(channel, 2),
                    tables::Control::TabOffset3 => Cea608::TabOffset(channel, 3),
                    tables::Control::DeleteToEndOfRow => Cea608::DeleteToEndOfRow(channel),
                    // TODO: TextRestart, ResumtTextDisplay
                    _ => {
                        if let Some(char) = code[0].char() {
                            Cea608::Text(Text {
                                char1: Some(char),
                                char2: None,
                                channel,
                            })
                        } else {
                            return Ok(None);
                        }
                    }
                }))
            }
            _ => {
                let Some(channel) = self.last_channel else {
                    return Ok(None);
                };
                let char1 = code[0].char();
                let char2 = code[1].char();
                if char1.is_some() || char2.is_some() {
                    Ok(Some(Cea608::Text(Text {
                        char1,
                        char2,
                        channel,
                    })))
                } else {
                    Ok(None)
                }
            }
        }
    }

    pub fn clear(&mut self) {
        *self = Self::default();
    }
}

#[cfg(test)]
mod test {
    use self::tables::ControlCode;

    use super::*;
    use crate::tests::*;

    #[test]
    fn state_duplicate_control() {
        test_init_log();
        let mut data = vec![];
        Code::Control(ControlCode::new(
            Channel(true),
            tables::Control::EraseDisplayedMemory,
        ))
        .write(&mut data)
        .unwrap();
        let mut state = Cea608State::default();
        assert_eq!(
            Ok(Some(Cea608::EraseDisplay(Channel(true)))),
            state.decode([data[0], data[1]])
        );
        assert_eq!(Ok(None), state.decode([data[0], data[1]]));
    }

    #[test]
    fn state_text_after_control() {
        test_init_log();
        let mut state = Cea608State::default();

        let mut data = vec![];
        Code::Control(ControlCode::new(Channel::ONE, tables::Control::RollUp2))
            .write(&mut data)
            .unwrap();
        assert_eq!(
            Ok(Some(Cea608::NewMode(Channel::ONE, Mode::RollUp2))),
            state.decode([data[0], data[1]])
        );

        let mut data = vec![];
        Code::LatinCapitalA.write(&mut data).unwrap();
        assert_eq!(
            Ok(Some(Cea608::Text(Text {
                char1: Some('A'),
                char2: None,
                channel: Channel::ONE,
            }))),
            state.decode([data[0], 0x80])
        );

        let mut data = vec![];
        Code::Control(ControlCode::new(Channel::TWO, tables::Control::RollUp2))
            .write(&mut data)
            .unwrap();
        assert_eq!(
            Ok(Some(Cea608::NewMode(Channel::TWO, Mode::RollUp2))),
            state.decode([data[0], data[1]])
        );

        let mut data = vec![];
        Code::LatinCapitalA.write(&mut data).unwrap();
        assert_eq!(
            Ok(Some(Cea608::Text(Text {
                char1: Some('A'),
                char2: None,
                channel: Channel::TWO,
            }))),
            state.decode([data[0], 0x80])
        );
    }
    /*
        #[test]
        fn simple_parse_dtvcc() {
            test_init_log();
            let data = [0x02, 0x01 << 5 | 0x01, 0x2A];
            let dtvcc = DTVCCPacket::parse(&data).unwrap();
            let services = dtvcc.services();
            assert_eq!(services.len(), 1);
            for service in services.iter() {
                assert_eq!(service.number, 1);
                let codes = service.codes();
                for code in codes.iter() {
                    trace!("parsed {code:?}");
                }
            }
        }

        #[test]
        fn simple_write_dtvcc() {
            test_init_log();
            let mut service = Service::new(1);
            let code = tables::Code::Asterisk;
            service.push_code(&code).unwrap();
            let mut dtvcc = DTVCCPacket::new(0);
            dtvcc.push_service(service).unwrap();
            let mut written = vec![];
            dtvcc.write(&mut written).unwrap();
            let data = [0x02, 0x01 << 5 | 0x01, 0x2A, 0x00];
            assert_eq!(written, data);
        }

        #[derive(Debug)]
        struct ServiceData<'a> {
            service_no: u8,
            codes: &'a [tables::Code],
        }

        #[derive(Debug)]
        struct PacketData<'a> {
            sequence_no: u8,
            services: &'a [ServiceData<'a>],
        }

        #[derive(Debug)]
        struct TestCCData<'a> {
            framerate: Framerate,
            cc_data: &'a [&'a [u8]],
            packets: &'a [PacketData<'a>],
            cea608: &'a [&'a [Cea608]],
        }

        static TEST_CC_DATA: [TestCCData; 6] = [
            // simple packet with a single service and single code
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[0x80 | 0x40 | 0x02, 0xFF, 0xFF, 0x02, 0x21, 0xFE, 0x41, 0x00]],
                packets: &[PacketData {
                    sequence_no: 0,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[tables::Code::LatinCapitalA],
                    }],
                }],
                cea608: &[],
            },
            // simple packet with a single service and two codes
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[0x80 | 0x40 | 0x02, 0xFF, 0xFF, 0x02, 0x22, 0xFE, 0x41, 0x42]],
                packets: &[PacketData {
                    sequence_no: 0,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[tables::Code::LatinCapitalA, tables::Code::LatinCapitalB],
                    }],
                }],
                cea608: &[],
            },
            // two packets each with a single service and single code
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[
                    &[0x80 | 0x40 | 0x02, 0xFF, 0xFF, 0x02, 0x21, 0xFE, 0x41, 0x00],
                    &[0x80 | 0x40 | 0x02, 0xFF, 0xFF, 0x42, 0x21, 0xFE, 0x42, 0x00],
                ],
                packets: &[
                    PacketData {
                        sequence_no: 0,
                        services: &[ServiceData {
                            service_no: 1,
                            codes: &[tables::Code::LatinCapitalA],
                        }],
                    },
                    PacketData {
                        sequence_no: 1,
                        services: &[ServiceData {
                            service_no: 1,
                            codes: &[tables::Code::LatinCapitalB],
                        }],
                    },
                ],
                cea608: &[],
            },
            // two packets with a single service and one code split across both packets
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[
                    &[0x80 | 0x40 | 0x01, 0xFF, 0xFF, 0x02, 0x21],
                    &[0x80 | 0x40 | 0x01, 0xFF, 0xFE, 0x41, 0x00],
                ],
                packets: &[PacketData {
                    sequence_no: 0,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[tables::Code::LatinCapitalA],
                    }],
                }],
                cea608: &[],
            },
            // simple packet with a single null service
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[0x80 | 0x40 | 0x01, 0xFF, 0xFF, 0x01, 0x00]],
                packets: &[PacketData {
                    sequence_no: 0,
                    services: &[],
                }],
                cea608: &[],
            },
            // two packets with a single service and one code split across both packets with 608
            // padding data
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[
                    &[
                        0x80 | 0x40 | 0x03,
                        0xFF,
                        0xFC,
                        0x61,
                        0x62,
                        0xFD,
                        0x63,
                        0x64,
                        0xFF,
                        0x02,
                        0x21,
                    ],
                    &[
                        0x80 | 0x40 | 0x03,
                        0xFF,
                        0xFC,
                        0x41,
                        0x42,
                        0xFD,
                        0x43,
                        0x44,
                        0xFE,
                        0x41,
                        0x00,
                    ],
                ],
                packets: &[PacketData {
                    sequence_no: 0,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[tables::Code::LatinCapitalA],
                    }],
                }],
                cea608: &[
                    &[Cea608::Field1(0x61, 0x62), Cea608::Field2(0x63, 0x64)],
                    &[Cea608::Field1(0x41, 0x42), Cea608::Field2(0x43, 0x44)],
                ],
            },
        ];

        #[test]
        fn cc_data_parse() {
            test_init_log();
            for (i, test_data) in TEST_CC_DATA.iter().enumerate() {
                info!("parsing {i}: {test_data:?}");
                let mut parser = CCDataParser::new();
                if !test_data.cea608.is_empty() {
                    parser.handle_cea608();
                }
                let mut expected_iter = test_data.packets.iter();
                let mut cea608_iter = test_data.cea608.iter();
                for data in test_data.cc_data.iter() {
                    debug!("pushing {data:?}");
                    parser.push(data).unwrap();
                    while let Some(packet) = parser.pop_packet() {
                        let expected = expected_iter.next().unwrap();
                        assert_eq!(expected.sequence_no, packet.sequence_no());
                        let services = packet.services();
                        let mut expected_service_iter = expected.services.iter();
                        for parsed_service in services.iter() {
                            let expected_service = expected_service_iter.next().unwrap();
                            assert_eq!(parsed_service.number(), expected_service.service_no);
                            assert_eq!(expected_service.codes, parsed_service.codes());
                        }
                        assert!(expected_service_iter.next().is_none());
                    }
                    assert_eq!(parser.cea608().as_ref(), cea608_iter.next());
                }
                assert!(parser.pop_packet().is_none());
                assert!(expected_iter.next().is_none());
                assert!(cea608_iter.next().is_none());
            }
        }

        static WRITE_CC_DATA: [TestCCData; 6] = [
            // simple packet with a single service and single code
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[0x80 | 0x40 | 0x02, 0xFF, 0xFF, 0x02, 0x21, 0xFE, 0x41, 0x00]],
                packets: &[PacketData {
                    sequence_no: 0,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[tables::Code::LatinCapitalA],
                    }],
                }],
                cea608: &[],
            },
            // simple packet with a single service and two codes
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[0x80 | 0x40 | 0x02, 0xFF, 0xFF, 0x02, 0x22, 0xFE, 0x41, 0x42]],
                packets: &[PacketData {
                    sequence_no: 0,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[tables::Code::LatinCapitalA, tables::Code::LatinCapitalB],
                    }],
                }],
                cea608: &[],
            },
            // packet with a full service service
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[
                    0x80 | 0x40 | 0x11,
                    0xFF,
                    0xFF,
                    0xC0 | 0x11,
                    0x20 | 0x1F,
                    0xFE,
                    0x41,
                    0x42,
                    0xFE,
                    0x43,
                    0x44,
                    0xFE,
                    0x45,
                    0x46,
                    0xFE,
                    0x47,
                    0x48,
                    0xFE,
                    0x49,
                    0x4A,
                    0xFE,
                    0x4B,
                    0x4C,
                    0xFE,
                    0x4D,
                    0x4E,
                    0xFE,
                    0x4F,
                    0x50,
                    0xFE,
                    0x51,
                    0x52,
                    0xFE,
                    0x53,
                    0x54,
                    0xFE,
                    0x55,
                    0x56,
                    0xFE,
                    0x57,
                    0x58,
                    0xFE,
                    0x59,
                    0x5A,
                    0xFE,
                    0x61,
                    0x62,
                    0xFE,
                    0x63,
                    0x64,
                    0xFE,
                    0x65,
                    0x0,
                ]],
                packets: &[PacketData {
                    sequence_no: 3,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[
                            tables::Code::LatinCapitalA,
                            tables::Code::LatinCapitalB,
                            tables::Code::LatinCapitalC,
                            tables::Code::LatinCapitalD,
                            tables::Code::LatinCapitalE,
                            tables::Code::LatinCapitalF,
                            tables::Code::LatinCapitalG,
                            tables::Code::LatinCapitalH,
                            tables::Code::LatinCapitalI,
                            tables::Code::LatinCapitalJ,
                            tables::Code::LatinCapitalK,
                            tables::Code::LatinCapitalL,
                            tables::Code::LatinCapitalM,
                            tables::Code::LatinCapitalN,
                            tables::Code::LatinCapitalO,
                            tables::Code::LatinCapitalP,
                            tables::Code::LatinCapitalQ,
                            tables::Code::LatinCapitalR,
                            tables::Code::LatinCapitalS,
                            tables::Code::LatinCapitalT,
                            tables::Code::LatinCapitalU,
                            tables::Code::LatinCapitalV,
                            tables::Code::LatinCapitalW,
                            tables::Code::LatinCapitalX,
                            tables::Code::LatinCapitalY,
                            tables::Code::LatinCapitalZ,
                            tables::Code::LatinLowerA,
                            tables::Code::LatinLowerB,
                            tables::Code::LatinLowerC,
                            tables::Code::LatinLowerD,
                            tables::Code::LatinLowerE,
                        ],
                    }],
                }],
                cea608: &[],
            },
            // simple packet with only cea608 data
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[0x80 | 0x40 | 0x01, 0xFF, 0xFC, 0x41, 0x42]],
                packets: &[],
                cea608: &[&[Cea608::Field1(0x41, 0x42)]],
            },
            // simple packet with only cea608 field 1 data
            TestCCData {
                framerate: Framerate::new(25, 1),
                cc_data: &[&[0x80 | 0x40 | 0x02, 0xFF, 0xFC, 0x80, 0x80, 0xFD, 0x41, 0x42]],
                packets: &[],
                cea608: &[&[Cea608::Field2(0x41, 0x42)]],
            },
            // simple packet that will span two outputs
            TestCCData {
                framerate: Framerate::new(60, 1),
                cc_data: &[
                    &[
                        0x80 | 0x40 | 0x0A,
                        0xFF,
                        0xFC,
                        0x20,
                        0x42,
                        0xFF,
                        0xC0 | 0x11,
                        0x20 | 0x1F,
                        0xFE,
                        0x41,
                        0x42,
                        0xFE,
                        0x43,
                        0x44,
                        0xFE,
                        0x45,
                        0x46,
                        0xFE,
                        0x47,
                        0x48,
                        0xFE,
                        0x49,
                        0x4A,
                        0xFE,
                        0x4B,
                        0x4C,
                        0xFE,
                        0x4D,
                        0x4E,
                        0xFE,
                        0x4F,
                        0x50,
                    ],
                    &[
                        0x80 | 0x40 | 0x09,
                        0xFF,
                        0xFD,
                        0x21,
                        0x43,
                        0xFE,
                        0x51,
                        0x52,
                        0xFE,
                        0x53,
                        0x54,
                        0xFE,
                        0x55,
                        0x56,
                        0xFE,
                        0x57,
                        0x58,
                        0xFE,
                        0x59,
                        0x5A,
                        0xFE,
                        0x61,
                        0x62,
                        0xFE,
                        0x63,
                        0x64,
                        0xFE,
                        0x65,
                        0x0,
                    ],
                ],
                packets: &[PacketData {
                    sequence_no: 3,
                    services: &[ServiceData {
                        service_no: 1,
                        codes: &[
                            tables::Code::LatinCapitalA,
                            tables::Code::LatinCapitalB,
                            tables::Code::LatinCapitalC,
                            tables::Code::LatinCapitalD,
                            tables::Code::LatinCapitalE,
                            tables::Code::LatinCapitalF,
                            tables::Code::LatinCapitalG,
                            tables::Code::LatinCapitalH,
                            tables::Code::LatinCapitalI,
                            tables::Code::LatinCapitalJ,
                            tables::Code::LatinCapitalK,
                            tables::Code::LatinCapitalL,
                            tables::Code::LatinCapitalM,
                            tables::Code::LatinCapitalN,
                            tables::Code::LatinCapitalO,
                            tables::Code::LatinCapitalP,
                            tables::Code::LatinCapitalQ,
                            tables::Code::LatinCapitalR,
                            tables::Code::LatinCapitalS,
                            tables::Code::LatinCapitalT,
                            tables::Code::LatinCapitalU,
                            tables::Code::LatinCapitalV,
                            tables::Code::LatinCapitalW,
                            tables::Code::LatinCapitalX,
                            tables::Code::LatinCapitalY,
                            tables::Code::LatinCapitalZ,
                            tables::Code::LatinLowerA,
                            tables::Code::LatinLowerB,
                            tables::Code::LatinLowerC,
                            tables::Code::LatinLowerD,
                            tables::Code::LatinLowerE,
                        ],
                    }],
                }],
                cea608: &[&[Cea608::Field1(0x20, 0x42), Cea608::Field2(0x21, 0x43)]],
            },
        ];

        #[test]
        fn packet_write_cc_data() {
            test_init_log();
            for test_data in WRITE_CC_DATA.iter() {
                info!("writing {test_data:?}");
                let mut packet_iter = test_data.packets.iter();
                let mut cea608_iter = test_data.cea608.iter();
                let mut writer = CCDataWriter::default();
                for cc_data in test_data.cc_data.iter() {
                    if let Some(packet_data) = packet_iter.next() {
                        let mut pack = DTVCCPacket::new(packet_data.sequence_no);
                        for service_data in packet_data.services.iter() {
                            let mut service = Service::new(service_data.service_no);
                            for code in service_data.codes.iter() {
                                service.push_code(code).unwrap();
                            }
                            pack.push_service(service).unwrap();
                        }
                        writer.push_packet(pack);
                    }
                    if let Some(&cea608) = cea608_iter.next() {
                        for pair in cea608 {
                            writer.push_cea608(*pair);
                        }
                    }
                    let mut written = vec![];
                    writer.write(test_data.framerate, &mut written).unwrap();
                    assert_eq!(cc_data, &written);
                }
            }
        }

        #[test]
        fn framerate_cea608_pairs_per_field() {
            assert_eq!(Framerate::new(60, 1).cea608_pairs_per_field(), 1);
            assert_eq!(Framerate::new(30, 1).cea608_pairs_per_field(), 2);
        }

        #[test]
        fn framerate_max_cc_count() {
            assert_eq!(Framerate::new(60, 1).max_cc_count(), 10);
            assert_eq!(Framerate::new(30, 1).max_cc_count(), 20);
        }

        #[test]
        fn framerate_new() {
            let fps = Framerate::new(30, 8);
            assert_eq!(fps.numer(), 30);
            assert_eq!(fps.denom(), 8);
        }
    */
}

#[cfg(test)]
pub(crate) mod tests {
    use once_cell::sync::Lazy;

    static TRACING: Lazy<()> = Lazy::new(|| {
        env_logger::init();
    });

    pub fn test_init_log() {
        Lazy::force(&TRACING);
    }
}
