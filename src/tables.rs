// Copyright (C) 2024 Matthew Waters <matthew@centricular.com>
//
// Licensed under the MIT license <LICENSE-MIT> or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Module for the [Code] table

/// Errors when parsing a [`Code`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum CodeError {
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

/// The channel the control code references
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Channel(pub(crate) bool);

impl Channel {
    /// Channel 1
    pub const ONE: Channel = Channel(true);
    /// Channel 2
    pub const TWO: Channel = Channel(false);

    /// The numerical identifier of this channel
    pub fn id(&self) -> u8 {
        if self.0 {
            1
        } else {
            2
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// must be ordered the same as the byte values
// These codes start with 0x11 (channel 1, odd-parity: 0x91) or 0x19 (channel 2, odd-parity: 0x19)
pub struct ControlCode {
    channel: Channel,
    control: Control,
}

impl ControlCode {
    pub fn new(channel: Channel, control: Control) -> Self {
        Self { channel, control }
    }

    pub fn channel(&self) -> Channel {
        self.channel
    }

    pub fn code(&self) -> Control {
        self.control
    }

    fn write(&self) -> [u8; 2] {
        let mut data;
        match self.control {
            Control::Unknown(unk) => {
                data = [unk[0], unk[1]];
            }
            Control::MidRow(midrow) => {
                data = midrow.to_bytes();
            }
            Control::PreambleAddress(preamble) => {
                data = preamble.to_bytes();
            }
            _ => {
                if let Ok(idx) = CONTROL_MAP_TABLE
                    .binary_search_by_key(&self.control, |control_map| control_map.control)
                {
                    data = CONTROL_MAP_TABLE[idx].cea608_bytes;
                } else {
                    unreachable!();
                }
            }
        }
        if self.channel == Channel::TWO {
            data[0] |= 0x08;
        }
        for data in data.iter_mut() {
            *data = add_parity(*data);
        }
        data
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum MidRowColor {
    Color(Color),
    Italics,
}

/// A mid-row change command
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MidRow {
    color: MidRowColor,
    underline: bool,
}

impl MidRow {
    /// Construct a new mid row command signalling a color
    pub fn new_color(color: Color, underline: bool) -> Self {
        Self {
            color: MidRowColor::Color(color),
            underline,
        }
    }

    /// Construct a new mid row command signalling italics
    pub fn new_italics(underline: bool) -> Self {
        Self {
            color: MidRowColor::Italics,
            underline,
        }
    }

    /// The color of this mid row command
    pub fn color(&self) -> Option<Color> {
        if let MidRowColor::Color(color) = self.color {
            Some(color)
        } else {
            None
        }
    }

    /// Whether underline is signalled with this mid row command
    pub fn underline(&self) -> bool {
        self.underline
    }

    /// Whether italics is signalled with this mid row command
    pub fn italics(&self) -> bool {
        matches!(self.color, MidRowColor::Italics)
    }

    fn to_bytes(self) -> [u8; 2] {
        let underline = if self.underline { 0x01 } else { 0x0 };
        let color = match self.color {
            MidRowColor::Color(Color::White) => 0x20,
            MidRowColor::Color(Color::Green) => 0x22,
            MidRowColor::Color(Color::Blue) => 0x24,
            MidRowColor::Color(Color::Cyan) => 0x26,
            MidRowColor::Color(Color::Red) => 0x28,
            MidRowColor::Color(Color::Yellow) => 0x2a,
            MidRowColor::Color(Color::Magenta) => 0x2c,
            MidRowColor::Italics => 0x2e,
        };
        [0x11, color + underline]
    }
}

/// The color options available
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Color {
    White,
    Green,
    Blue,
    Cyan,
    Red,
    Yellow,
    Magenta,
}

/// Enum representing control commands
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// must be ordered the same as the byte values
// These codes start with 0x11 (channel 1, odd-parity: 0x91) or 0x19 (channel 2, odd-parity: 0x19)
pub enum Control {
    MidRow(MidRow),
    RegisteredTrademarkSign,
    DegreeSign,
    Fraction12,
    InvertedQuestionMark,
    TradeMarkSign,
    CentSign,
    PoundSign,
    MusicalNote,
    LatinLowerAWithGrave,
    TransparentSpace,
    LatinLowerEWithGrave,
    LatinLowerAWithCircumflex,
    LatinLowerEWithCircumflex,
    LatinLowerIWithCircumflex,
    LatinLowerOWithCircumflex,
    LatinLowerUWithCircumflex,

    LatinCapitalAWithAcute,
    LatinCapitalEWithAcute,
    LatinCapitalOWithAcute,
    LatinCapitalUWithAcute,
    LatinCapitalUWithDiaeseresis,
    LatinLowerUWithDiaeseresis,
    OpeningSingleQuote,
    InvertedExclamationMark,
    Asterisk,
    SingleOpenQuote,
    EmDash,
    CopyrightSign,
    ServiceMarkSign,
    RoundBullet,
    DoubleOpenQuote,
    DoubleCloseQuote,
    LatinCapitalAWithGrave,
    LatinCapitalAWithCircumflex,
    LatinCapitalCWithCedilla,
    LatinCapitalEWithGrave,
    LatinCapitalEWithCircumflex,
    LatinCapitalEWithDiaeresis,
    LatinLowerEWithDiaeresis,
    LatinCapitalIWithCircumflex,
    LatinCapitalIWithDiaeresis,
    LatinLowerIWithDiaeresis,
    LatinCapitalOWithCircumflex,
    LatinCapitalUWithGrave,
    LatinLowerUWithGrave,
    LatinCapitalUWithCircumflex,
    OpeningGuillemets,
    ClosingGuillemets,

    LatinCapitalAWithTilde,
    LatinLowerAWithTilde,
    LatinCapitalIWithAcute,
    LatinCapitalIWithGrave,
    LatinLowerIWithGrave,
    LatinCapitalOWithGrave,
    LatinLowerOWithGrave,
    LatinCapitalOWithTilde,
    LatinLowerOWithTilde,
    OpeningBrace,
    ClosingBrace,
    ReverseSolidus,
    Caret,
    Underbar,
    Pipe,
    Tilde,
    LatinCapitalAWithDiaeresis,
    LatinLowerAWithDiaeresis,
    LatinCapitalOWithDiaeresis,
    LatinLowerOWithDiaeresis,
    LatinLowerSharpS,
    YenSign,
    GeneralCurrencySign,
    VerticalBar,
    LatinCapitalAWithRingAbove,
    LatinLowerAWithRingAbove,
    LatinCapitalOWithStroke,
    LatinLowerOWithStroke,
    UpperLeftBorder,
    UpperRightBorder,
    LowerLeftBorder,
    LowerRightBorder,

    ResumeCaptionLoading,
    Backspace,
    AlarmOff,
    AlarmOn,
    DeleteToEndOfRow,
    RollUp2,
    RollUp3,
    RollUp4,
    FlashOn,
    ResumeDirectionCaptioning,
    TextRestart,
    ResumeTextDisplay,
    EraseDisplayedMemory,
    CarriageReturn,
    EraseNonDisplayedMemory,
    EndOfCaption,

    TabOffset1,
    TabOffset2,
    TabOffset3,

    PreambleAddress(PreambleAddressCode),
    Unknown([u8; 2]),
}

/// A preamble address code command contents
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PreambleAddressCode {
    row: u8,
    underline: bool,
    ty: PreambleType,
}

impl PreambleAddressCode {
    /// Construct a new preamble
    pub fn new(base_row: u8, underline: bool, code: PreambleType) -> Self {
        Self {
            row: base_row,
            underline,
            ty: code,
        }
    }

    /// The row specified in this preamble
    pub fn row(&self) -> u8 {
        self.row
    }

    /// The column specified in this preamble
    pub fn column(&self) -> u8 {
        match self.ty {
            PreambleType::Indent0 => 0,
            PreambleType::Indent4 => 4,
            PreambleType::Indent8 => 8,
            PreambleType::Indent12 => 12,
            PreambleType::Indent16 => 16,
            PreambleType::Indent20 => 20,
            PreambleType::Indent24 => 24,
            PreambleType::Indent28 => 28,
            _ => 0,
        }
    }

    /// Whether underline is signaled in this preamble
    pub fn underline(&self) -> bool {
        self.underline
    }

    /// The complete preamble code
    pub fn code(&self) -> PreambleType {
        self.ty
    }

    /// Whether italics is signaled in this preamble
    pub fn italics(&self) -> bool {
        matches!(self.ty, PreambleType::WhiteItalics)
    }

    /// The color of this preamble
    pub fn color(&self) -> Color {
        if let PreambleType::Color(color) = self.ty {
            color
        } else {
            // all indents assign white as the color
            Color::White
        }
    }

    fn to_bytes(self) -> [u8; 2] {
        let underline = if self.underline { 0x1 } else { 0x0 };
        let (row0, row1) = match self.row {
            1 => (0x11, 0x40),
            2 => (0x11, 0x60),
            3 => (0x12, 0x40),
            4 => (0x12, 0x60),
            5 => (0x15, 0x40),
            6 => (0x15, 0x60),
            7 => (0x16, 0x40),
            8 => (0x16, 0x60),
            9 => (0x17, 0x40),
            10 => (0x17, 0x60),
            11 => (0x10, 0x40),
            12 => (0x13, 0x40),
            13 => (0x13, 0x60),
            14 => (0x14, 0x40),
            15 => (0x14, 0x60),
            _ => unreachable!(),
        };
        let ty = match self.ty {
            PreambleType::Color(Color::White) => 0x00,
            PreambleType::Color(Color::Green) => 0x02,
            PreambleType::Color(Color::Blue) => 0x04,
            PreambleType::Color(Color::Cyan) => 0x06,
            PreambleType::Color(Color::Red) => 0x08,
            PreambleType::Color(Color::Yellow) => 0x0a,
            PreambleType::Color(Color::Magenta) => 0x0c,
            PreambleType::WhiteItalics => 0x0e,
            PreambleType::Indent0 => 0x10,
            PreambleType::Indent4 => 0x12,
            PreambleType::Indent8 => 0x14,
            PreambleType::Indent12 => 0x16,
            PreambleType::Indent16 => 0x18,
            PreambleType::Indent20 => 0x1a,
            PreambleType::Indent24 => 0x1c,
            PreambleType::Indent28 => 0x1e,
        };
        [row0, row1 | ty | underline]
    }
}

/// The type of the preamble
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PreambleType {
    Color(Color),
    WhiteItalics,
    Indent0,
    Indent4,
    Indent8,
    Indent12,
    Indent16,
    Indent20,
    Indent24,
    Indent28,
}

/// Enum of all possible characters or commands available
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// must be ordered the same as the byte values for binary search to be successful
pub enum Code {
    NUL,
    Control(ControlCode),
    Space, // 0x20
    ExclamationMark,
    QuotationMark,
    NumberSign,
    DollarSign,
    PercentSign,
    Ampersand,
    Apostrophe,
    LeftParenthesis,
    RightParenthesis,
    LatinLowerAWithAcute,
    PlusSign,
    Comma,
    HyphenMinus,
    FullStop,
    Solidus,
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Colon,
    SemiColon,
    LessThan,
    Equals,
    GreaterThan,
    QuestionMark,
    CommercialAt,
    LatinCapitalA,
    LatinCapitalB,
    LatinCapitalC,
    LatinCapitalD,
    LatinCapitalE,
    LatinCapitalF,
    LatinCapitalG,
    LatinCapitalH,
    LatinCapitalI,
    LatinCapitalJ,
    LatinCapitalK,
    LatinCapitalL,
    LatinCapitalM,
    LatinCapitalN,
    LatinCapitalO,
    LatinCapitalP,
    LatinCapitalQ,
    LatinCapitalR,
    LatinCapitalS,
    LatinCapitalT,
    LatinCapitalU,
    LatinCapitalV,
    LatinCapitalW,
    LatinCapitalX,
    LatinCapitalY,
    LatinCapitalZ,
    LeftSquareBracket,
    LatinLowerEWithAcute,
    RightSquareBracket,
    LatinLowerIWithAcute,
    LatinLowerOWithAcute,
    LatinLowerUWithAcute,
    LatinLowerA,
    LatinLowerB,
    LatinLowerC,
    LatinLowerD,
    LatinLowerE,
    LatinLowerF,
    LatinLowerG,
    LatinLowerH,
    LatinLowerI,
    LatinLowerJ,
    LatinLowerK,
    LatinLowerL,
    LatinLowerM,
    LatinLowerN,
    LatinLowerO,
    LatinLowerP,
    LatinLowerQ,
    LatinLowerR,
    LatinLowerS,
    LatinLowerT,
    LatinLowerU,
    LatinLowerV,
    LatinLowerW,
    LatinLowerX,
    LatinLowerY,
    LatinLowerZ,
    LatinLowerCWithCedilla,
    DivisionSign,
    LatinCapitalNWithTilde,
    LatinLowerNWithTilde,
    SolidBlock, // 0x7F

    Unknown(u8),
}

#[derive(Debug, Clone)]
struct CodeMap<'a> {
    pub cea608_bytes: &'a [u8],
    pub code: Code,
    pub utf8: Option<char>,
}

macro_rules! code_map_bytes {
    ($bytes:expr, $code:expr, $utf8:expr) => {
        CodeMap {
            cea608_bytes: &$bytes,
            code: $code,
            utf8: $utf8,
        }
    };
}

macro_rules! code_map_single_byte {
    ($byte:expr, $code:expr, $utf8:expr) => {
        code_map_bytes!([$byte], $code, $utf8)
    };
}

// needs to be sorted by bytes and Code
static CODE_MAP_TABLE: [CodeMap; 97] = [
    code_map_single_byte!(0x00, Code::NUL, None),
    code_map_single_byte!(0x20, Code::Space, Some(' ')),
    code_map_single_byte!(0x21, Code::ExclamationMark, Some('!')),
    code_map_single_byte!(0x22, Code::QuotationMark, Some('\"')),
    code_map_single_byte!(0x23, Code::NumberSign, Some('#')),
    code_map_single_byte!(0x24, Code::DollarSign, Some('$')),
    code_map_single_byte!(0x25, Code::PercentSign, Some('%')),
    code_map_single_byte!(0x26, Code::Ampersand, Some('&')),
    code_map_single_byte!(0x27, Code::Apostrophe, Some('\'')),
    code_map_single_byte!(0x28, Code::LeftParenthesis, Some('(')),
    code_map_single_byte!(0x29, Code::RightParenthesis, Some(')')),
    code_map_single_byte!(0x2A, Code::LatinLowerAWithAcute, Some('Á')),
    //code_map_single_byte!(0x2A, Code::Asterisk, Some('*')),
    code_map_single_byte!(0x2B, Code::PlusSign, Some('+')),
    code_map_single_byte!(0x2C, Code::Comma, Some(',')),
    code_map_single_byte!(0x2D, Code::HyphenMinus, Some('-')),
    code_map_single_byte!(0x2E, Code::FullStop, Some('.')),
    code_map_single_byte!(0x2F, Code::Solidus, Some('/')),
    code_map_single_byte!(0x30, Code::Zero, Some('0')),
    code_map_single_byte!(0x31, Code::One, Some('1')),
    code_map_single_byte!(0x32, Code::Two, Some('2')),
    code_map_single_byte!(0x33, Code::Three, Some('3')),
    code_map_single_byte!(0x34, Code::Four, Some('4')),
    code_map_single_byte!(0x35, Code::Five, Some('5')),
    code_map_single_byte!(0x36, Code::Six, Some('6')),
    code_map_single_byte!(0x37, Code::Seven, Some('7')),
    code_map_single_byte!(0x38, Code::Eight, Some('8')),
    code_map_single_byte!(0x39, Code::Nine, Some('9')),
    code_map_single_byte!(0x3A, Code::Colon, Some(':')),
    code_map_single_byte!(0x3B, Code::SemiColon, Some(';')),
    code_map_single_byte!(0x3C, Code::LessThan, Some('<')),
    code_map_single_byte!(0x3D, Code::Equals, Some('=')),
    code_map_single_byte!(0x3E, Code::GreaterThan, Some('>')),
    code_map_single_byte!(0x3F, Code::QuestionMark, Some('?')),
    code_map_single_byte!(0x40, Code::CommercialAt, Some('@')),
    code_map_single_byte!(0x41, Code::LatinCapitalA, Some('A')),
    code_map_single_byte!(0x42, Code::LatinCapitalB, Some('B')),
    code_map_single_byte!(0x43, Code::LatinCapitalC, Some('C')),
    code_map_single_byte!(0x44, Code::LatinCapitalD, Some('D')),
    code_map_single_byte!(0x45, Code::LatinCapitalE, Some('E')),
    code_map_single_byte!(0x46, Code::LatinCapitalF, Some('F')),
    code_map_single_byte!(0x47, Code::LatinCapitalG, Some('G')),
    code_map_single_byte!(0x48, Code::LatinCapitalH, Some('H')),
    code_map_single_byte!(0x49, Code::LatinCapitalI, Some('I')),
    code_map_single_byte!(0x4A, Code::LatinCapitalJ, Some('J')),
    code_map_single_byte!(0x4B, Code::LatinCapitalK, Some('K')),
    code_map_single_byte!(0x4C, Code::LatinCapitalL, Some('L')),
    code_map_single_byte!(0x4D, Code::LatinCapitalM, Some('M')),
    code_map_single_byte!(0x4E, Code::LatinCapitalN, Some('N')),
    code_map_single_byte!(0x4F, Code::LatinCapitalO, Some('O')),
    code_map_single_byte!(0x50, Code::LatinCapitalP, Some('P')),
    code_map_single_byte!(0x51, Code::LatinCapitalQ, Some('Q')),
    code_map_single_byte!(0x52, Code::LatinCapitalR, Some('R')),
    code_map_single_byte!(0x53, Code::LatinCapitalS, Some('S')),
    code_map_single_byte!(0x54, Code::LatinCapitalT, Some('T')),
    code_map_single_byte!(0x55, Code::LatinCapitalU, Some('U')),
    code_map_single_byte!(0x56, Code::LatinCapitalV, Some('V')),
    code_map_single_byte!(0x57, Code::LatinCapitalW, Some('W')),
    code_map_single_byte!(0x58, Code::LatinCapitalX, Some('X')),
    code_map_single_byte!(0x59, Code::LatinCapitalY, Some('Y')),
    code_map_single_byte!(0x5A, Code::LatinCapitalZ, Some('Z')),
    code_map_single_byte!(0x5B, Code::LeftSquareBracket, Some('[')),
    code_map_single_byte!(0x5C, Code::LatinLowerEWithAcute, Some('é')),
    code_map_single_byte!(0x5D, Code::RightSquareBracket, Some(']')),
    code_map_single_byte!(0x5E, Code::LatinLowerIWithAcute, Some('í')),
    code_map_single_byte!(0x5F, Code::LatinLowerOWithAcute, Some('ó')),
    code_map_single_byte!(0x60, Code::LatinLowerUWithAcute, Some('ú')),
    code_map_single_byte!(0x61, Code::LatinLowerA, Some('a')),
    code_map_single_byte!(0x62, Code::LatinLowerB, Some('b')),
    code_map_single_byte!(0x63, Code::LatinLowerC, Some('c')),
    code_map_single_byte!(0x64, Code::LatinLowerD, Some('d')),
    code_map_single_byte!(0x65, Code::LatinLowerE, Some('e')),
    code_map_single_byte!(0x66, Code::LatinLowerF, Some('f')),
    code_map_single_byte!(0x67, Code::LatinLowerG, Some('g')),
    code_map_single_byte!(0x68, Code::LatinLowerH, Some('h')),
    code_map_single_byte!(0x69, Code::LatinLowerI, Some('i')),
    code_map_single_byte!(0x6A, Code::LatinLowerJ, Some('j')),
    code_map_single_byte!(0x6B, Code::LatinLowerK, Some('k')),
    code_map_single_byte!(0x6C, Code::LatinLowerL, Some('l')),
    code_map_single_byte!(0x6D, Code::LatinLowerM, Some('m')),
    code_map_single_byte!(0x6E, Code::LatinLowerN, Some('n')),
    code_map_single_byte!(0x6F, Code::LatinLowerO, Some('o')),
    code_map_single_byte!(0x70, Code::LatinLowerP, Some('p')),
    code_map_single_byte!(0x71, Code::LatinLowerQ, Some('q')),
    code_map_single_byte!(0x72, Code::LatinLowerR, Some('r')),
    code_map_single_byte!(0x73, Code::LatinLowerS, Some('s')),
    code_map_single_byte!(0x74, Code::LatinLowerT, Some('t')),
    code_map_single_byte!(0x75, Code::LatinLowerU, Some('u')),
    code_map_single_byte!(0x76, Code::LatinLowerV, Some('v')),
    code_map_single_byte!(0x77, Code::LatinLowerW, Some('w')),
    code_map_single_byte!(0x78, Code::LatinLowerX, Some('x')),
    code_map_single_byte!(0x79, Code::LatinLowerY, Some('y')),
    code_map_single_byte!(0x7A, Code::LatinLowerZ, Some('z')),
    code_map_single_byte!(0x7B, Code::LatinLowerCWithCedilla, Some('Ç')),
    code_map_single_byte!(0x7C, Code::DivisionSign, Some('÷')),
    code_map_single_byte!(0x7D, Code::LatinCapitalNWithTilde, Some('Ñ')),
    code_map_single_byte!(0x7E, Code::LatinLowerNWithTilde, Some('ñ')),
    code_map_single_byte!(0x7F, Code::SolidBlock, Some('█')),
];

#[derive(Debug, Clone)]
struct ControlMap {
    cea608_bytes: [u8; 2],
    control: Control,
    utf8: Option<char>,
}

macro_rules! control_map_bytes {
    ($bytes:expr, $control:expr, $utf8:expr) => {
        ControlMap {
            cea608_bytes: $bytes,
            control: $control,
            utf8: $utf8,
        }
    };
}

static CONTROL_MAP_TABLE: [ControlMap; 99] = [
    control_map_bytes!([0x11, 0x30], Control::RegisteredTrademarkSign, Some('Ⓡ')),
    control_map_bytes!([0x11, 0x31], Control::DegreeSign, Some('°')),
    control_map_bytes!([0x11, 0x32], Control::Fraction12, Some('½')),
    control_map_bytes!([0x11, 0x33], Control::InvertedQuestionMark, Some('¿')),
    control_map_bytes!([0x11, 0x34], Control::TradeMarkSign, Some('™')),
    control_map_bytes!([0x11, 0x35], Control::CentSign, Some('¢')),
    control_map_bytes!([0x11, 0x36], Control::PoundSign, Some('£')),
    control_map_bytes!([0x11, 0x37], Control::MusicalNote, Some('♪')),
    control_map_bytes!([0x11, 0x38], Control::LatinLowerAWithGrave, Some('à')),
    control_map_bytes!([0x11, 0x39], Control::TransparentSpace, None),
    control_map_bytes!([0x11, 0x3a], Control::LatinLowerEWithGrave, Some('è')),
    control_map_bytes!([0x11, 0x3b], Control::LatinLowerAWithCircumflex, Some('â')),
    control_map_bytes!([0x11, 0x3c], Control::LatinLowerEWithCircumflex, Some('ê')),
    control_map_bytes!([0x11, 0x3d], Control::LatinLowerIWithCircumflex, Some('î')),
    control_map_bytes!([0x11, 0x3e], Control::LatinLowerOWithCircumflex, Some('ô')),
    control_map_bytes!([0x11, 0x3f], Control::LatinLowerUWithCircumflex, Some('û')),
    control_map_bytes!([0x12, 0x20], Control::LatinCapitalAWithAcute, Some('Á')),
    control_map_bytes!([0x12, 0x21], Control::LatinCapitalEWithAcute, Some('É')),
    control_map_bytes!([0x12, 0x22], Control::LatinCapitalOWithAcute, Some('Ó')),
    control_map_bytes!([0x12, 0x23], Control::LatinCapitalUWithAcute, Some('Ú')),
    control_map_bytes!(
        [0x12, 0x24],
        Control::LatinCapitalUWithDiaeseresis,
        Some('Ü')
    ),
    control_map_bytes!([0x12, 0x25], Control::LatinLowerUWithDiaeseresis, Some('ü')),
    control_map_bytes!([0x12, 0x26], Control::OpeningSingleQuote, Some('´')),
    control_map_bytes!([0x12, 0x27], Control::InvertedExclamationMark, Some('´')),
    control_map_bytes!([0x12, 0x28], Control::Asterisk, Some('*')),
    control_map_bytes!([0x12, 0x29], Control::SingleOpenQuote, Some('\'')),
    control_map_bytes!([0x12, 0x2a], Control::EmDash, Some('_')),
    control_map_bytes!([0x12, 0x2b], Control::CopyrightSign, Some('Ⓒ')),
    control_map_bytes!([0x12, 0x2c], Control::ServiceMarkSign, Some('℠')),
    control_map_bytes!([0x12, 0x2d], Control::RoundBullet, None),
    control_map_bytes!([0x12, 0x2e], Control::DoubleOpenQuote, Some('“')),
    control_map_bytes!([0x12, 0x2f], Control::DoubleCloseQuote, Some('”')),
    control_map_bytes!([0x12, 0x30], Control::LatinCapitalAWithGrave, Some('À')),
    control_map_bytes!(
        [0x12, 0x31],
        Control::LatinCapitalAWithCircumflex,
        Some('Â')
    ),
    control_map_bytes!([0x12, 0x32], Control::LatinCapitalCWithCedilla, Some('Ç')),
    control_map_bytes!([0x12, 0x33], Control::LatinCapitalEWithGrave, Some('È')),
    control_map_bytes!(
        [0x12, 0x34],
        Control::LatinCapitalEWithCircumflex,
        Some('Ê')
    ),
    control_map_bytes!([0x12, 0x35], Control::LatinCapitalEWithDiaeresis, Some('Ë')),
    control_map_bytes!([0x12, 0x36], Control::LatinLowerEWithDiaeresis, Some('ë')),
    control_map_bytes!(
        [0x12, 0x37],
        Control::LatinCapitalIWithCircumflex,
        Some('Î')
    ),
    control_map_bytes!([0x12, 0x38], Control::LatinCapitalIWithDiaeresis, Some('Ï')),
    control_map_bytes!([0x12, 0x39], Control::LatinLowerIWithDiaeresis, Some('ï')),
    control_map_bytes!(
        [0x12, 0x3a],
        Control::LatinCapitalOWithCircumflex,
        Some('Ô')
    ),
    control_map_bytes!([0x12, 0x3b], Control::LatinCapitalUWithGrave, Some('Ù')),
    control_map_bytes!([0x12, 0x3c], Control::LatinLowerUWithGrave, Some('ù')),
    control_map_bytes!(
        [0x12, 0x3d],
        Control::LatinCapitalUWithCircumflex,
        Some('Û')
    ),
    control_map_bytes!([0x12, 0x3e], Control::OpeningGuillemets, Some('«')),
    control_map_bytes!([0x12, 0x3f], Control::ClosingGuillemets, Some('»')),
    control_map_bytes!([0x13, 0x20], Control::LatinCapitalAWithTilde, Some('Ã')),
    control_map_bytes!([0x13, 0x21], Control::LatinLowerAWithTilde, Some('ã')),
    control_map_bytes!([0x13, 0x22], Control::LatinCapitalIWithAcute, Some('Í')),
    control_map_bytes!([0x13, 0x23], Control::LatinCapitalIWithGrave, Some('Ì')),
    control_map_bytes!([0x13, 0x24], Control::LatinLowerIWithGrave, Some('ì')),
    control_map_bytes!([0x13, 0x25], Control::LatinCapitalOWithGrave, Some('Ò')),
    control_map_bytes!([0x13, 0x26], Control::LatinLowerOWithGrave, Some('ò')),
    control_map_bytes!([0x13, 0x27], Control::LatinCapitalOWithTilde, Some('Õ')),
    control_map_bytes!([0x13, 0x28], Control::LatinLowerOWithTilde, Some('õ')),
    control_map_bytes!([0x13, 0x29], Control::OpeningBrace, Some('{')),
    control_map_bytes!([0x13, 0x2a], Control::ClosingBrace, Some('}')),
    control_map_bytes!([0x13, 0x2b], Control::ReverseSolidus, Some('\\')),
    control_map_bytes!([0x13, 0x2c], Control::Caret, Some('^')),
    control_map_bytes!([0x13, 0x2d], Control::Underbar, Some('_')),
    control_map_bytes!([0x13, 0x2e], Control::Pipe, Some('|')),
    control_map_bytes!([0x13, 0x2f], Control::Tilde, Some('~')),
    control_map_bytes!([0x13, 0x30], Control::LatinCapitalAWithDiaeresis, Some('Ä')),
    control_map_bytes!([0x13, 0x31], Control::LatinLowerAWithDiaeresis, Some('ä')),
    control_map_bytes!([0x13, 0x32], Control::LatinCapitalOWithDiaeresis, Some('Ö')),
    control_map_bytes!([0x13, 0x33], Control::LatinLowerOWithDiaeresis, Some('ö')),
    control_map_bytes!([0x13, 0x34], Control::LatinLowerSharpS, Some('ß')),
    control_map_bytes!([0x13, 0x35], Control::YenSign, Some('¥')),
    control_map_bytes!([0x13, 0x36], Control::GeneralCurrencySign, Some('¤')),
    control_map_bytes!([0x13, 0x37], Control::VerticalBar, Some('¦')),
    control_map_bytes!([0x13, 0x38], Control::LatinCapitalAWithRingAbove, Some('Å')),
    control_map_bytes!([0x13, 0x39], Control::LatinLowerAWithRingAbove, Some('å')),
    control_map_bytes!([0x13, 0x3a], Control::LatinCapitalOWithStroke, Some('Ø')),
    control_map_bytes!([0x13, 0x3b], Control::LatinLowerOWithStroke, Some('ø')),
    control_map_bytes!([0x13, 0x3c], Control::UpperLeftBorder, None),
    control_map_bytes!([0x13, 0x3d], Control::UpperRightBorder, None),
    control_map_bytes!([0x13, 0x3e], Control::LowerLeftBorder, None),
    control_map_bytes!([0x13, 0x3f], Control::LowerRightBorder, None),
    control_map_bytes!([0x14, 0x20], Control::ResumeCaptionLoading, None),
    control_map_bytes!([0x14, 0x21], Control::Backspace, None),
    control_map_bytes!([0x14, 0x22], Control::AlarmOff, None),
    control_map_bytes!([0x14, 0x23], Control::AlarmOn, None),
    control_map_bytes!([0x14, 0x24], Control::DeleteToEndOfRow, None),
    control_map_bytes!([0x14, 0x25], Control::RollUp2, None),
    control_map_bytes!([0x14, 0x26], Control::RollUp3, None),
    control_map_bytes!([0x14, 0x27], Control::RollUp4, None),
    control_map_bytes!([0x14, 0x28], Control::FlashOn, None),
    control_map_bytes!([0x14, 0x29], Control::ResumeDirectionCaptioning, None),
    control_map_bytes!([0x14, 0x2a], Control::TextRestart, None),
    control_map_bytes!([0x14, 0x2b], Control::ResumeTextDisplay, None),
    control_map_bytes!([0x14, 0x2c], Control::EraseDisplayedMemory, None),
    control_map_bytes!([0x14, 0x2d], Control::CarriageReturn, None),
    control_map_bytes!([0x14, 0x2e], Control::EraseNonDisplayedMemory, None),
    control_map_bytes!([0x14, 0x2f], Control::EndOfCaption, None),
    control_map_bytes!([0x17, 0x21], Control::TabOffset1, None),
    control_map_bytes!([0x17, 0x22], Control::TabOffset2, None),
    control_map_bytes!([0x17, 0x23], Control::TabOffset3, None),
];

fn strip_parity(byte: u8) -> u8 {
    byte & 0x7F
}

fn add_parity(byte: u8) -> u8 {
    debug_assert!((byte & 0x80) == 0);
    if check_odd_parity(byte) {
        byte
    } else {
        byte | 0x80
    }
}

fn check_odd_parity(byte: u8) -> bool {
    byte.count_ones() % 2 == 1
}

fn parse_control_code(data: [u8; 2]) -> ControlCode {
    let channel = data[0] & 0x08;
    let underline = data[1] & 0x1 != 0;

    ControlCode {
        channel: Channel(channel == 0),
        control: match (data[0] & !0x08, data[1]) {
            (0x11, 0x20 | 0x21) => Control::MidRow(MidRow {
                color: MidRowColor::Color(Color::White),
                underline,
            }),
            (0x11, 0x22 | 0x23) => Control::MidRow(MidRow {
                color: MidRowColor::Color(Color::Green),
                underline,
            }),
            (0x11, 0x24 | 0x25) => Control::MidRow(MidRow {
                color: MidRowColor::Color(Color::Blue),
                underline,
            }),
            (0x11, 0x26 | 0x27) => Control::MidRow(MidRow {
                color: MidRowColor::Color(Color::Cyan),
                underline,
            }),
            (0x11, 0x28 | 0x29) => Control::MidRow(MidRow {
                color: MidRowColor::Color(Color::Red),
                underline,
            }),
            (0x11, 0x2a | 0x2b) => Control::MidRow(MidRow {
                color: MidRowColor::Color(Color::Yellow),
                underline,
            }),
            (0x11, 0x2c | 0x2d) => Control::MidRow(MidRow {
                color: MidRowColor::Color(Color::Magenta),
                underline,
            }),
            (0x11, 0x2e | 0x2f) => Control::MidRow(MidRow {
                color: MidRowColor::Italics,
                underline,
            }),
            (0x10..=0x19, 0x20..=0x3f) => {
                let idx = CONTROL_MAP_TABLE
                    .binary_search_by_key(&[data[0] & !0x08, data[1]], |control_map| {
                        control_map.cea608_bytes
                    });
                idx.map(|idx| CONTROL_MAP_TABLE[idx].control)
                    .unwrap_or_else(|_| Control::Unknown(data))
            }
            (byte0, 0x40..=0x7f) => {
                if let Some(preamble) = parse_preamble(byte0, data[1]) {
                    Control::PreambleAddress(preamble)
                } else {
                    Control::Unknown(data)
                }
            }
            _ => Control::Unknown(data),
        },
    }
}

fn parse_preamble(byte0: u8, byte1: u8) -> Option<PreambleAddressCode> {
    let underline = byte1 & 0x1 != 0;
    let row = match (byte0, byte1) {
        (0x11, 0x40..=0x5f) => 1,
        (0x11, 0x60..=0x7f) => 2,
        (0x12, 0x40..=0x5f) => 3,
        (0x12, 0x60..=0x7f) => 4,
        (0x15, 0x40..=0x5f) => 5,
        (0x15, 0x60..=0x7f) => 6,
        (0x16, 0x40..=0x5f) => 7,
        (0x16, 0x60..=0x7f) => 8,
        (0x17, 0x40..=0x5f) => 9,
        (0x17, 0x60..=0x7f) => 10,
        (0x10, 0x40..=0x5f) => 11,
        (0x13, 0x40..=0x5f) => 12,
        (0x13, 0x60..=0x7f) => 13,
        (0x14, 0x40..=0x5f) => 14,
        (0x14, 0x60..=0x7f) => 15,
        _ => return None,
    };
    let ty = match byte1 & 0x1e {
        0x00 => PreambleType::Color(Color::White),
        0x02 => PreambleType::Color(Color::Green),
        0x04 => PreambleType::Color(Color::Blue),
        0x06 => PreambleType::Color(Color::Cyan),
        0x08 => PreambleType::Color(Color::Red),
        0x0a => PreambleType::Color(Color::Yellow),
        0x0c => PreambleType::Color(Color::Magenta),
        0x0e => PreambleType::WhiteItalics,
        0x10 => PreambleType::Indent0,
        0x12 => PreambleType::Indent4,
        0x14 => PreambleType::Indent8,
        0x16 => PreambleType::Indent12,
        0x18 => PreambleType::Indent16,
        0x1a => PreambleType::Indent20,
        0x1c => PreambleType::Indent24,
        0x1e => PreambleType::Indent28,
        _ => return None,
    };
    Some(PreambleAddressCode { row, underline, ty })
}

impl Code {
    /// The length in bytes of this [Code]
    ///
    /// # Examples
    /// ```
    /// # use cea608_types::tables::Code;
    /// assert_eq!(Code::LatinCapitalA.byte_len(), 1);
    /// ```
    pub fn byte_len(&self) -> usize {
        match self {
            Code::Control(_) => 2,
            _ => 1,
        }
    }

    /// Parse a byte sequence into a list of [Code]s
    ///
    /// # Examples
    /// ```
    /// # use cea608_types::tables::Code;
    /// assert_eq!(Code::from_data([0xC1, 0x80]), Ok([Code::LatinCapitalA, Code::NUL]));
    /// ```
    pub fn from_data(data: [u8; 2]) -> Result<[Code; 2], CodeError> {
        if !check_odd_parity(data[0]) {
            return Err(CodeError::InvalidParity);
        }
        if !check_odd_parity(data[1]) {
            return Err(CodeError::InvalidParity);
        }
        let data = [strip_parity(data[0]), strip_parity(data[1])];

        if (0x10..=0x1F).contains(&data[0]) {
            Ok([Code::Control(parse_control_code(data)), Code::NUL])
        } else {
            let code0 = CODE_MAP_TABLE
                .binary_search_by_key(&[data[0]].as_slice(), |code_map| code_map.cea608_bytes);
            let code1 = CODE_MAP_TABLE
                .binary_search_by_key(&[data[1]].as_slice(), |code_map| code_map.cea608_bytes);
            Ok([
                code0
                    .map(|idx| CODE_MAP_TABLE[idx].code)
                    .unwrap_or_else(|_| Code::Unknown(data[0])),
                code1
                    .map(|idx| CODE_MAP_TABLE[idx].code)
                    .unwrap_or_else(|_| Code::Unknown(data[1])),
            ])
        }
    }

    /// Write a [Code] to a byte stream
    ///
    /// # Examples
    /// ```
    /// # use cea608_types::tables::Code;
    /// let mut written = vec![];
    /// Code::LatinCapitalC.write(&mut written).unwrap();
    /// assert_eq!(written, [0x43]);
    /// ```
    pub fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        match self {
            Code::Unknown(data) => {
                return w.write_all(&[add_parity(*data)]);
            }
            Code::Control(control) => return w.write_all(&control.write()),
            _ => {
                if let Ok(idx) =
                    CODE_MAP_TABLE.binary_search_by_key(&self, |code_map| &code_map.code)
                {
                    let data = CODE_MAP_TABLE[idx]
                        .cea608_bytes
                        .iter()
                        .map(|b| add_parity(*b))
                        .collect::<Vec<_>>();
                    return w.write_all(&data);
                }
            }
        }
        unreachable!()
    }

    /// The utf8 char for this [Code]
    ///
    /// [Code]s that represent a command will return None.
    ///
    /// # Examples
    /// ```
    /// # use cea608_types::tables::Code;
    /// assert_eq!(Code::LatinCapitalA.char(), Some('A'));
    /// ```
    pub fn char(&self) -> Option<char> {
        // table is not currently sorted by utf8 value so cannot binary search through it.  May
        // need another lookup table if this is a performance concern
        if let Code::Control(ControlCode { control, .. }) = self {
            return CONTROL_MAP_TABLE.iter().find_map(|control_map| {
                if control_map.control == *control {
                    control_map.utf8
                } else {
                    None
                }
            });
        }

        CODE_MAP_TABLE.iter().find_map(|code_map| {
            if code_map.code == *self {
                code_map.utf8
            } else {
                None
            }
        })
    }

    /// Retrieve a [Code] for a utf8 char
    ///
    /// If the char is not representable as a [Code], None will be returned.
    ///
    /// # Examples
    /// ```
    /// # use cea608_types::tables::{Code, Channel};
    /// assert_eq!(Code::from_char('A', Channel::ONE), Some(Code::LatinCapitalA));
    /// ```
    pub fn from_char(c: char, channel: Channel) -> Option<Code> {
        // table is not currently sorted by utf8 value so cannot binary search through it.  May
        // need another lookup table if this is a performance concern
        CODE_MAP_TABLE.iter().find_map(|code_map| {
            if code_map.utf8 == Some(c) {
                Some(code_map.code)
            } else {
                CONTROL_MAP_TABLE.iter().find_map(|control_map| {
                    if code_map.utf8 == Some(c) {
                        Some(Code::Control(ControlCode {
                            channel,
                            control: control_map.control,
                        }))
                    } else {
                        None
                    }
                })
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests::*;

    #[test]
    fn codes_table_ordered() {
        test_init_log();
        let mut iter = CODE_MAP_TABLE.iter().peekable();
        while let Some(code_map) = iter.next() {
            if let Some(peek) = iter.peek() {
                trace!("checking ordinality for {code_map:?} and {peek:?}");
                assert!(peek.code > code_map.code);
                assert!(peek.cea608_bytes > code_map.cea608_bytes);
            }
        }
    }

    #[test]
    fn control_table_ordered() {
        test_init_log();
        let mut iter = CONTROL_MAP_TABLE.iter().peekable();
        while let Some(control_map) = iter.next() {
            if let Some(peek) = iter.peek() {
                trace!("checking ordinality for {control_map:?} and {peek:?}");
                assert!(peek.control > control_map.control);
                assert!(peek.cea608_bytes > control_map.cea608_bytes);
            }
        }
    }

    #[test]
    fn codes_to_from_bytes() {
        test_init_log();
        for code_map in CODE_MAP_TABLE.iter() {
            trace!("parsing {code_map:?}");
            let mut data = Vec::from_iter(code_map.cea608_bytes.iter().map(|b| add_parity(*b)));
            data.resize(2, 0x80);
            let parsed_code = Code::from_data(data.try_into().unwrap()).unwrap();
            assert_eq!(parsed_code[0], code_map.code);
            let mut written = vec![];
            parsed_code[0].write(&mut written).unwrap();
            assert_eq!(written.len(), code_map.code.byte_len());
            let written = written.iter().map(|b| strip_parity(*b)).collect::<Vec<_>>();
            assert_eq!(written, code_map.cea608_bytes);
        }
    }

    #[test]
    fn codes_to_from_char() {
        test_init_log();
        for code_map in CODE_MAP_TABLE.iter() {
            trace!("parsing {code_map:?}");
            if let Some(c) = code_map.utf8 {
                let parsed_code = Code::from_char(c, Channel(true)).unwrap();
                assert_eq!(parsed_code.char(), code_map.utf8);
                assert_eq!(parsed_code, code_map.code);
                let mut written = vec![];
                parsed_code.write(&mut written).unwrap();
                let written = written.iter().map(|b| strip_parity(*b)).collect::<Vec<_>>();
                assert_eq!(written, code_map.cea608_bytes);
            }
        }
    }

    #[test]
    fn preamble_to_from_bytes() {
        test_init_log();
        let tys = [
            PreambleType::Color(Color::White),
            PreambleType::Color(Color::Green),
            PreambleType::Color(Color::Blue),
            PreambleType::Color(Color::Cyan),
            PreambleType::Color(Color::Red),
            PreambleType::Color(Color::Yellow),
            PreambleType::Color(Color::Magenta),
            PreambleType::WhiteItalics,
            PreambleType::Indent0,
            PreambleType::Indent4,
            PreambleType::Indent8,
            PreambleType::Indent12,
            PreambleType::Indent16,
            PreambleType::Indent20,
            PreambleType::Indent24,
            PreambleType::Indent28,
        ];
        for row in 1..=15 {
            for underline in [true, false] {
                for ty in tys {
                    for channel in [Channel::ONE, Channel::TWO] {
                        let preamble = Code::Control(ControlCode {
                            channel,
                            control: Control::PreambleAddress(PreambleAddressCode {
                                row,
                                underline,
                                ty,
                            }),
                        });
                        debug!("{preamble:?}");
                        let mut data = vec![];
                        preamble.write(&mut data).unwrap();
                        debug!("{data:x?}");
                        let parsed = Code::from_data([data[0], data[1]]).unwrap();
                        assert_eq!(preamble, parsed[0]);
                    }
                }
            }
        }
    }

    #[test]
    fn midrow_to_from_bytes() {
        test_init_log();
        let colors = [
            MidRowColor::Color(Color::White),
            MidRowColor::Color(Color::Green),
            MidRowColor::Color(Color::Blue),
            MidRowColor::Color(Color::Cyan),
            MidRowColor::Color(Color::Red),
            MidRowColor::Color(Color::Yellow),
            MidRowColor::Color(Color::Magenta),
            MidRowColor::Italics,
        ];
        for underline in [true, false] {
            for color in colors {
                for channel in [Channel::ONE, Channel::TWO] {
                    let midrow = Code::Control(ControlCode {
                        channel,
                        control: Control::MidRow(MidRow { underline, color }),
                    });
                    debug!("{midrow:?}");
                    let mut data = vec![];
                    midrow.write(&mut data).unwrap();
                    debug!("{data:x?}");
                    let parsed = Code::from_data([data[0], data[1]]).unwrap();
                    assert_eq!(midrow, parsed[0]);
                }
            }
        }
    }
}
