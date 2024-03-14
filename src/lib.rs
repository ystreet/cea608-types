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

use std::collections::VecDeque;

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

/// A CEA-08 presentation mode
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Mode {
    PopOn,
    PaintOn,
    RollUp2,
    RollUp3,
    RollUp4,
}

impl Mode {
    /// Whether this mode is a roll-up mode
    pub fn is_rollup(&self) -> bool {
        matches!(self, Self::RollUp2 | Self::RollUp3 | Self::RollUp4)
    }
}

/// Text information
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Text {
    /// Whether the character needs the remove the previous character.
    pub needs_backspace: bool,
    /// Optional character 1
    pub char1: Option<char>,
    /// Optional character 2
    pub char2: Option<char>,
    /// The last channel received
    pub channel: Channel,
}

/// CEA-08 information
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Cea608 {
    /// Text
    Text(Text),
    /// The channel is changing (or resending) mode
    NewMode(Channel, Mode),
    /// Erase the currently displayed window contents
    EraseDisplay(Channel),
    /// Erase the undisplayed window contents
    EraseNonDisplay(Channel),
    /// A carriage return was received
    CarriageReturn(Channel),
    /// A backspace was received
    Backspace(Channel),
    /// An end of caption was received.  In Pop-On mode, swap the undisplayed and displayed window
    /// contents
    EndOfCaption(Channel),
    /// Offset the cursor
    TabOffset(Channel, u8),
    /// Delete characters from the current cursor position to the end of the row
    DeleteToEndOfRow(Channel),
    /// A preamble was received
    Preamble(Channel, PreambleAddressCode),
    /// A mid-row was received
    MidRowChange(Channel, MidRow),
}

impl Cea608 {
    /// The channel for this parsed CEA-608 data
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
    /// Decode the provided bytes into an optional parsed [`Cea608`] command.
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
                    // TODO: TextRestart, ResumeTextDisplay
                    _ => {
                        if let Some(char) = code[0].char() {
                            Cea608::Text(Text {
                                needs_backspace: code[0].needs_backspace(),
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
                        needs_backspace: false,
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

    /// Reset the state to that of an initially constructed object.
    pub fn reset(&mut self) {
        *self = Self::default();
    }
}

/// A writer that handles combining single byte [`Code`]s and double byte [`Code`]s.
#[derive(Debug, Default)]
pub struct Cea608Writer {
    pending: VecDeque<Code>,
    pending_code: Option<Code>,
}

impl Cea608Writer {
    /// Push a [`Code`] into this writer
    pub fn push(&mut self, code: Code) {
        self.pending.push_front(code)
    }

    /// Pop a [`Code`] from this writer
    pub fn pop(&mut self) -> [u8; 2] {
        let mut ret = [0x80; 2];
        let mut prev = None::<Code>;

        if let Some(code) = self.pending_code.take() {
            code.write_into(&mut ret);
            return ret;
        }

        while let Some(code) = self.pending.pop_back() {
            if let Some(prev) = prev {
                if code.byte_len() == 1 {
                    let mut data = [0; 2];
                    prev.write_into(&mut ret);
                    code.write_into(&mut data);
                    ret[1] = data[0];
                    return ret;
                } else if code.needs_backspace() {
                    self.pending_code = Some(code);
                    let mut data = [0; 2];
                    prev.write_into(&mut ret);
                    Code::Space.write_into(&mut data);
                    ret[1] = data[0];
                    return ret;
                } else {
                    self.pending_code = Some(code);
                    prev.write_into(&mut ret);
                    return ret;
                }
            } else if code.needs_backspace() {
                // all back space needing codes are 2 byte commands
                self.pending_code = Some(code);
                Code::Space.write_into(&mut ret);
                return ret;
            } else if code.byte_len() == 1 {
                prev = Some(code);
            } else {
                code.write_into(&mut ret);
                return ret;
            }
        }
        if let Some(prev) = prev {
            prev.write_into(&mut ret);
        }
        ret
    }

    /// The number of codes currently stored
    pub fn n_codes(&self) -> usize {
        self.pending.len() + if self.pending_code.is_some() { 1 } else { 0 }
    }

    /// Reset as if it was a newly created instance
    pub fn reset(&mut self) {
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
                needs_backspace: false,
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
                needs_backspace: false,
                char1: Some('A'),
                char2: None,
                channel: Channel::TWO,
            }))),
            state.decode([data[0], 0x80])
        );
    }

    #[test]
    fn writer_padding() {
        test_init_log();
        let mut writer = Cea608Writer::default();
        assert_eq!(writer.pop(), [0x80, 0x80]);
    }

    #[test]
    fn writer_single_byte_code() {
        test_init_log();
        let mut writer = Cea608Writer::default();
        writer.push(Code::LatinLowerA);
        assert_eq!(writer.pop(), [0x61, 0x80]);
        assert_eq!(writer.pop(), [0x80, 0x80]);
    }

    #[test]
    fn writer_two_single_byte_codes() {
        test_init_log();
        let mut writer = Cea608Writer::default();
        writer.push(Code::LatinLowerA);
        writer.push(Code::LatinLowerB);
        assert_eq!(writer.pop(), [0x61, 0x62]);
        assert_eq!(writer.pop(), [0x80, 0x80]);
    }

    #[test]
    fn writer_single_byte_and_control() {
        test_init_log();
        let mut writer = Cea608Writer::default();
        writer.push(Code::LatinLowerA);
        writer.push(Code::Control(ControlCode::new(
            Channel::ONE,
            tables::Control::DegreeSign,
        )));
        assert_eq!(writer.pop(), [0x61, 0x80]);
        assert_eq!(writer.pop(), [0x91, 0x31]);
        assert_eq!(writer.pop(), [0x80, 0x80]);
    }

    #[test]
    fn writer_single_byte_and_control_needing_backspace() {
        test_init_log();
        let mut writer = Cea608Writer::default();
        writer.push(Code::LatinLowerA);
        writer.push(Code::Control(ControlCode::new(
            Channel::ONE,
            tables::Control::Tilde,
        )));
        assert_eq!(writer.pop(), [0x61, 0x20]);
        assert_eq!(writer.pop(), [0x13, 0x2f]);
        assert_eq!(writer.pop(), [0x80, 0x80]);
    }

    #[test]
    fn writer_control_needing_backspace() {
        test_init_log();
        let mut writer = Cea608Writer::default();
        writer.push(Code::Control(ControlCode::new(
            Channel::ONE,
            tables::Control::Tilde,
        )));
        assert_eq!(writer.pop(), [0x20, 0x80]);
        assert_eq!(writer.pop(), [0x13, 0x2f]);
        assert_eq!(writer.pop(), [0x80, 0x80]);
    }

    #[test]
    fn writer_control() {
        test_init_log();
        let mut writer = Cea608Writer::default();
        writer.push(Code::Control(ControlCode::new(
            Channel::ONE,
            tables::Control::DegreeSign,
        )));
        assert_eq!(writer.pop(), [0x91, 0x31]);
        assert_eq!(writer.pop(), [0x80, 0x80]);
    }
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
