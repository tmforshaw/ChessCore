use crate::board::Player;

pub const PIECE_AMT: usize = 6;
pub const COLOUR_AMT: usize = 2;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Piece {
    BQueen = 0,
    BKing = 1,
    BRook = 2,
    BKnight = 3,
    BBishop = 4,
    BPawn = 5,
    WQueen = 8,
    WKing = 9,
    WRook = 10,
    WKnight = 11,
    WBishop = 12,
    WPawn = 13,
    None = 14,
}

impl From<Piece> for usize {
    fn from(value: Piece) -> Self {
        value as Self - 1 - 2 * Self::from(value.is_black())
    }
}

impl From<usize> for Piece {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::BQueen,
            1 => Self::BKing,
            2 => Self::BRook,
            3 => Self::BKnight,
            4 => Self::BBishop,
            5 => Self::BPawn,
            8 => Self::WQueen,
            9 => Self::WKing,
            10 => Self::WRook,
            11 => Self::WKnight,
            12 => Self::WBishop,
            13 => Self::WPawn,
            _ => Self::None,
        }
    }
}

pub const PIECES: &[Piece] = &[
    Piece::BQueen,
    Piece::BKing,
    Piece::BRook,
    Piece::BKnight,
    Piece::BBishop,
    Piece::BPawn,
    Piece::WQueen,
    Piece::WKing,
    Piece::WRook,
    Piece::WKnight,
    Piece::WBishop,
    Piece::WPawn,
];

impl Piece {
    #[must_use]
    pub fn is_white(self) -> bool {
        ((self as u8 >> 3) & 1) == 1 && self != Self::None
    }

    #[must_use]
    pub fn is_black(self) -> bool {
        ((self as u8 >> 3) & 1) == 0 && self != Self::None
    }

    #[must_use]
    pub const fn to_player(self) -> Option<Player> {
        match self {
            Self::BQueen
            | Self::BKing
            | Self::BRook
            | Self::BKnight
            | Self::BBishop
            | Self::BPawn => Some(Player::Black),

            Self::WQueen
            | Self::WKing
            | Self::WRook
            | Self::WKnight
            | Self::WBishop
            | Self::WPawn => Some(Player::White),

            Self::None => None,
        }
    }

    #[must_use]
    pub fn is_player(self, player: Player) -> bool {
        match player {
            Player::White => self.is_white(),
            Player::Black => self.is_black(),
        }
    }

    /// # Panics
    /// Panics if ``Piece::None`` is used as an index
    #[must_use]
    pub fn to_bitboard_index(&self) -> usize {
        PIECES
            .iter()
            .enumerate()
            .find_map(|(i, piece)| if self == piece { Some(i) } else { None })
            .expect("Piece::None cannot be converted to a bitboard index")
    }

    #[must_use]
    pub const fn to_algebraic(&self) -> char {
        match self {
            Self::None => '-',
            Self::WPawn => 'P',
            Self::WKnight => 'N',
            Self::WBishop => 'B',
            Self::WRook => 'R',
            Self::WQueen => 'Q',
            Self::WKing => 'K',
            Self::BPawn => 'p',
            Self::BKnight => 'n',
            Self::BBishop => 'b',
            Self::BRook => 'r',
            Self::BQueen => 'q',
            Self::BKing => 'k',
        }
    }

    #[must_use]
    pub const fn from_algebraic(chr: char) -> Option<Self> {
        match chr {
            '-' => Some(Self::None),
            'P' => Some(Self::WPawn),
            'N' => Some(Self::WKnight),
            'B' => Some(Self::WBishop),
            'R' => Some(Self::WRook),
            'Q' => Some(Self::WQueen),
            'K' => Some(Self::WKing),
            'p' => Some(Self::BPawn),
            'n' => Some(Self::BKnight),
            'b' => Some(Self::BBishop),
            'r' => Some(Self::BRook),
            'q' => Some(Self::BQueen),
            'k' => Some(Self::BKing),
            _ => None,
        }
    }

    #[must_use]
    pub const fn get_player_piece(player: Player, piece: Piece) -> Piece {
        match player {
            Player::White => match piece {
                Piece::WQueen | Piece::BQueen => Piece::WQueen,
                Piece::WKing | Piece::BKing => Piece::WKing,
                Piece::WRook | Piece::BRook => Piece::WRook,
                Piece::WKnight | Piece::BKnight => Piece::WKnight,
                Piece::WBishop | Piece::BBishop => Piece::WBishop,
                Piece::WPawn | Piece::BPawn => Piece::WPawn,
                Piece::None => Piece::None,
            },
            Player::Black => match piece {
                Piece::WQueen | Piece::BQueen => Piece::BQueen,
                Piece::WKing | Piece::BKing => Piece::BKing,
                Piece::WRook | Piece::BRook => Piece::BRook,
                Piece::WKnight | Piece::BKnight => Piece::BKnight,
                Piece::WBishop | Piece::BBishop => Piece::BBishop,
                Piece::WPawn | Piece::BPawn => Piece::BPawn,
                Piece::None => Piece::None,
            },
        }
    }
}

impl From<Piece> for char {
    fn from(val: Piece) -> Self {
        val.to_algebraic()
    }
}
