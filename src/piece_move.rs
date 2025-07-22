use std::fmt;

use crate::{board::TilePos, piece::Piece};

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub enum PieceMoveType {
    #[default]
    Normal,
    EnPassant,
    Castling,
    Promotion(Piece),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PieceMove {
    pub from: TilePos,
    pub to: TilePos,
    pub move_type: PieceMoveType,
    pub show: bool,
}

impl PieceMove {
    #[must_use]
    pub const fn new(from: TilePos, to: TilePos) -> Self {
        Self {
            from,
            to,
            move_type: PieceMoveType::Normal,
            show: true,
        }
    }

    #[must_use]
    pub const fn with_show(&self, show: bool) -> Self {
        Self {
            from: self.from,
            to: self.to,
            move_type: self.move_type,
            show,
        }
    }

    pub const fn give_show(&mut self, show: bool) {
        self.show = show;
    }

    #[must_use]
    pub const fn with_castling(&self) -> Self {
        Self {
            from: self.from,
            to: self.to,
            move_type: PieceMoveType::Castling,
            show: self.show,
        }
    }

    pub const fn give_castling(&mut self) {
        self.move_type = PieceMoveType::Castling;
    }

    #[must_use]
    pub const fn with_en_passant_capture(&self) -> Self {
        Self {
            from: self.from,
            to: self.to,
            move_type: PieceMoveType::EnPassant,
            show: self.show,
        }
    }

    pub const fn give_en_passant_capture(&mut self) {
        self.move_type = PieceMoveType::EnPassant;
    }

    #[must_use]
    pub const fn with_promotion(&self, promoted_to: Piece) -> Self {
        Self {
            from: self.from,
            to: self.to,
            move_type: PieceMoveType::Promotion(promoted_to),
            show: self.show,
        }
    }

    pub const fn give_promotion(&mut self, promoted_to: Piece) {
        self.move_type = PieceMoveType::Promotion(promoted_to);
    }

    /// # Errors
    /// Returns an error if the from and to tiles contain files which cannot be converted to integers
    pub fn to_algebraic(&self) -> Result<String, std::num::TryFromIntError> {
        Ok(format!("{}{}", self.from.to_algebraic()?, self.to.to_algebraic()?))
    }

    /// # Errors
    /// Returns an error if the from and to tiles contain files which cannot be converted to integers
    pub fn from_algebraic(piece_move: &str) -> Result<Self, String> {
        let piece_move = String::from(piece_move.trim());

        let mut move_chars = Vec::new();

        let mut tiles = Vec::new();
        for chr in piece_move.chars() {
            match chr {
                'a'..='h' => move_chars.push(chr),
                '1'..='8' => {
                    move_chars.push(chr);
                    tiles.push(TilePos::from((
                        move_chars[0] as u32 - 'a' as u32,
                        move_chars[1] as u32 - '1' as u32,
                    )));

                    move_chars.clear();
                }
                _ => {
                    // Could not convert from algebraic
                    return Err(format!("Could not convert '{piece_move}' to TilePos"));
                }
            }
        }

        if tiles.len() == 2 {
            return Ok(Self::new(tiles[0], tiles[1]));
        }

        Err(format!("Not enough positions for PieceMove\t\t{tiles:?}"))
    }

    #[must_use]
    pub const fn rev(&self) -> Self {
        Self {
            from: self.to,
            to: self.from,
            move_type: self.move_type,
            show: self.show,
        }
    }
}

impl std::fmt::Debug for PieceMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{from: {}, to: {}, show: {}, move_type: {:?}}}",
            self.from, self.to, self.show, self.move_type
        )
    }
}

impl std::fmt::Display for PieceMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}, {}, {}, {:?}}}", self.from, self.to, self.show, self.move_type)
    }
}
