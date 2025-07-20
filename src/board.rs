use std::fmt;

use crate::{
    bitboards::BitBoards,
    move_history::{HistoryMove, PieceMoveHistory},
    piece::{COLOUR_AMT, Piece},
    piece_move::{PieceMove, PieceMoveType},
};

pub const BOARD_SIZE: u32 = 8;

#[derive(Default, Clone, Copy, Debug, Eq, PartialEq)]
pub enum Player {
    #[default]
    White,
    Black,
}

impl Player {
    /// # Panics
    // Panics if the PLAYERS array does not contain the Player
    #[must_use]
    pub fn to_index(&self) -> usize {
        PLAYERS
            .iter()
            .enumerate()
            .find_map(
                |(i, test_player)| {
                    if test_player == self { Some(i) } else { None }
                },
            )
            .unwrap_or_else(|| panic!("Could not find index of player: {self:?}"))
    }

    #[must_use]
    pub const fn next_player(&self) -> Self {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }
}

pub const PLAYERS: &[Player] = &[Player::White, Player::Black];

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct TilePos {
    pub file: u32,
    pub rank: u32,
}

impl TilePos {
    #[must_use]
    pub const fn new(file: u32, rank: u32) -> Self {
        Self { file, rank }
    }

    #[must_use]
    pub const fn to_index(&self) -> u32 {
        self.file + self.rank * BOARD_SIZE
    }

    #[must_use]
    pub const fn from_index(index: u32) -> Self {
        Self {
            file: index % BOARD_SIZE,
            rank: index / BOARD_SIZE,
        }
    }

    /// # Errors
    /// Throws a ``TryFromIntError`` if the file cannot be converted to an integer
    pub fn to_algebraic(&self) -> Result<String, std::num::TryFromIntError> {
        Ok(format!(
            "{}{}",
            (b'a' + u8::try_from(self.file)?) as char,
            self.rank + 1
        ))
    }
}

impl std::fmt::Debug for TilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(file: {}, rank: {})", self.file, self.rank)
    }
}

impl std::fmt::Display for TilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.file, self.rank)
    }
}

impl From<(u32, u32)> for TilePos {
    fn from((file, rank): (u32, u32)) -> Self {
        Self::new(file, rank)
    }
}

impl From<TilePos> for (u32, u32) {
    fn from(value: TilePos) -> Self {
        (value.file, value.rank)
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Board {
    pub positions: BitBoards,
    pub player: Player,
    // pub castling_rights: [(bool, bool); COLOUR_AMT],
    // pub en_passant_on_last_move: Option<TilePos>,
    pub half_move_counter: usize,
    pub full_move_counter: usize,
    pub move_history: PieceMoveHistory,
}

impl Default for Board {
    fn default() -> Self {
        // const DEFAULT_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; // Normal Starting Board

        // const DEFAULT_FEN: &str = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"; // Castling Test Board

        // const DEFAULT_FEN: &str = "rnbqkbnr/p1p1pppp/1p6/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"; // En Pasasnt Test Board

        const DEFAULT_FEN: &str =
            "rnbqkbnr/1ppp1ppp/8/p3p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 4"; // Scholar's Mate Board

        // const DEFAULT_FEN: &str = "8/1ppkp1P1/3pp3/8/8/5PP1/p2PPKP1/8 w - - 1 1"; // Promotion Test Board

        // const DEFAULT_FEN: &str = "rn1qk1nr/pPppppPp/8/8/8/8/PpPPPPpP/RN1QK1NR w KQkq - 0 1"; // Capture Promotion Corner

        // const DEFAULT_FEN: &str = "rnbqkbnr/pppppppp/8/8/6r1/7p/7P/7K b - - 1 1"; // Stalemate Test Board

        match Self::from_fen(DEFAULT_FEN) {
            Ok(board) => board,
            Err(e) => panic!("Board could not be created from FEN:\n\t{e:?}"),
        }
    }
}

impl Board {
    /// # Errors
    /// Returns an error if FEN is incorrect
    pub fn from_fen<T: AsRef<str>>(fen_string: T) -> Result<Self, String> {
        let fen = fen_string.as_ref();

        let mut section_index = 0;

        let mut rank = 0;
        let mut file = 0;

        let mut board = Self {
            positions: BitBoards::default(),
            player: Player::default(),
            half_move_counter: 0,
            full_move_counter: 1,
            move_history: PieceMoveHistory::default(),
        };

        for (chr_index, chr) in fen.char_indices() {
            match section_index {
                // Read positions from FEN
                0 => match chr {
                    '/' => {
                        file = 0;
                        rank += 1;
                    }
                    '1'..='8' => file += u32::from(chr as u8 - b'0'),
                    ' ' => section_index += 1,
                    _ => {
                        if let Some(piece) = Piece::from_algebraic(chr) {
                            let tile_pos = TilePos::new(file, BOARD_SIZE - 1 - rank); // Count from the bottom (need to flip rank)
                            board.positions.set_piece(tile_pos, piece);
                            board.positions[piece].set_bit_at(tile_pos, true);

                            file += 1;
                        } else {
                            return Err(format!(
                                "Could not create board using FEN string [{fen}]:\n'{chr}' is not algebraic notation for any piece"
                            ));
                        }
                    }
                },
                // Read the current player's turn from FEN
                1 => match chr {
                    'w' => board.player = Player::White,
                    'b' => board.player = Player::Black,
                    ' ' => section_index += 1,
                    _ => {
                        return Err(format!(
                            "Could not create board using FEN string [{fen}]:\n'{chr}' is not a valid player"
                        ));
                    }
                },
                // Read the castling rights from FEN
                2 => match chr {
                    'K' => board.positions.castling_rights[Player::White as usize].0 = true,
                    'Q' => board.positions.castling_rights[Player::White as usize].1 = true,
                    'k' => board.positions.castling_rights[Player::Black as usize].0 = true,
                    'q' => board.positions.castling_rights[Player::Black as usize].1 = true,
                    '-' => board.positions.castling_rights = [(false, false); COLOUR_AMT],
                    ' ' => section_index += 1,
                    _ => {
                        return Err(format!(
                            "Could not create board using FEN string [{fen}]:\n'{chr}' does not provide valid castling rights information"
                        ));
                    }
                },
                // Reached the en passant part of FEN
                3 => match chr {
                    '-' => board.positions.en_passant_tile = 0,
                    ' ' => section_index += 1,
                    c => {
                        if !c.is_ascii_digit() {
                            let algebraic_en_passant =
                                fen.chars().skip(chr_index).take(2).collect::<Vec<_>>();

                            match (algebraic_en_passant[0], algebraic_en_passant[1]) {
                                ('a'..='h', '0'..='8') => {
                                    board.positions.en_passant_tile = 1
                                        << TilePos::new(
                                            u32::from(algebraic_en_passant[0] as u8 - b'a'),
                                            u32::from(algebraic_en_passant[1] as u8 - b'1'),
                                        )
                                        .to_index();
                                }
                                _ => {
                                    return Err(format!(
                                        "Could not create board using FEN string [{fen}]:\n\"{}{}\" is not a valid en passant square",
                                        algebraic_en_passant[0], algebraic_en_passant[1]
                                    ));
                                }
                            }
                        }
                    }
                },
                _ => break,
            }
        }

        Ok(board)
    }

    /// # Panics
    // Panics if en passant, castling, or promotion was not handled correctly
    pub fn apply_move(&mut self, mut piece_move: PieceMove) -> PieceMoveType {
        self.positions
            .apply_move(&mut piece_move, &mut self.move_history);

        self.next_player();

        piece_move.move_type
    }

    /// # Panics
    /// Panics if castling cannot be undone, or if piece couldn't un-promote
    pub fn undo_move(&mut self, history_move: HistoryMove) -> Option<HistoryMove> {
        self.positions.undo_move(history_move);

        // Only increment the player if the game didn't end on this move
        if self.positions.has_game_ended().is_none() {
            self.next_player();
        }

        Some(history_move)
    }

    #[must_use]
    pub const fn get_player(&self) -> Player {
        self.player
    }

    #[must_use]
    pub const fn get_next_player(&self) -> Player {
        match self.player {
            Player::White => Player::Black,
            Player::Black => Player::White,
        }
    }

    pub const fn next_player(&mut self) {
        self.player = self.get_next_player();
    }

    #[must_use]
    pub fn double_pawn_move_check(piece: Piece, from: TilePos) -> bool {
        (piece.is_white() && from.rank == 1) || (piece.is_black() && from.rank == BOARD_SIZE - 2)
    }

    #[must_use]
    pub fn get_vertical_dir(piece: Piece) -> isize {
        isize::from(piece.is_white()) * 2 - 1
    }

    #[must_use]
    pub fn get_tiles_between(&self, pos1: TilePos, pos2: TilePos) -> Option<Vec<TilePos>> {
        if pos1.file == pos2.file && pos1.rank == pos2.rank {
            return None;
        }

        let file_diff_isize = isize::try_from(pos1.file).ok()? - isize::try_from(pos2.file).ok()?;
        let rank_diff_isize = isize::try_from(pos1.rank).ok()? - isize::try_from(pos2.rank).ok()?;

        if file_diff_isize.unsigned_abs() > 0 && rank_diff_isize.unsigned_abs() > 0 {
            return None;
        }

        let lower_pos = if file_diff_isize < 0 || rank_diff_isize < 0 {
            pos1
        } else {
            pos2
        };

        let file_diff = u32::from(file_diff_isize != 0);
        let rank_diff = u32::from(rank_diff_isize != 0);

        Some(
            (1..((file_diff_isize.unsigned_abs() as u32)
                .max(rank_diff_isize.unsigned_abs() as u32)))
                .map(|k| {
                    TilePos::new(
                        lower_pos.file + k * file_diff,
                        lower_pos.rank + k * rank_diff,
                    )
                })
                .collect::<Vec<_>>(),
        )
    }
}
