use std::fmt;

use crate::{
    board::{BOARD_SIZE, Board, TilePos},
    piece::{COLOUR_AMT, Piece},
};

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

    #[must_use]
    pub const fn with_castling(&self) -> Self {
        Self {
            from: self.from,
            to: self.to,
            move_type: PieceMoveType::Castling,
            show: self.show,
        }
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

    #[must_use]
    pub const fn with_promotion(&self, promoted_to: Piece) -> Self {
        Self {
            from: self.from,
            to: self.to,
            move_type: PieceMoveType::Promotion(promoted_to),
            show: self.show,
        }
    }

    /// # Errors
    /// Returns an error if the from and to tiles contain files which cannot be converted to integers
    pub fn to_algebraic(&self) -> Result<String, std::num::TryFromIntError> {
        Ok(format!(
            "{}{}",
            self.from.to_algebraic()?,
            self.to.to_algebraic()?
        ))
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
        write!(
            f,
            "{{{}, {}, {}, {:?}}}",
            self.from, self.to, self.show, self.move_type
        )
    }
}

pub fn apply_promotion(
    board: &mut Board,
    moved_piece: Piece,
    mut piece_move: PieceMove,
) -> PieceMove {
    // Pawn was moved onto final file (Player doesn't matter here since pawn cannot move backwards)
    if moved_piece == Piece::get_player_piece(board.get_player(), Piece::WPawn)
        && (piece_move.to.rank == BOARD_SIZE - 1 || piece_move.to.rank == 0)
    {
        let promoted_to = match piece_move.move_type {
            PieceMoveType::Promotion(promoted_to) => promoted_to,
            _ => Piece::get_player_piece(board.get_player(), Piece::WQueen), // TODO Allow choosing which piece to promote to
        };

        piece_move = piece_move.with_promotion(promoted_to);

        perform_promotion(board, piece_move.from, promoted_to);
    }

    piece_move
}

pub fn perform_promotion(board: &mut Board, from: TilePos, new_piece_type: Piece) {
    // Change the type of the piece in the internal board
    board.positions.set_piece(from, new_piece_type);
}

// Returns the en_passant tile for this move
/// # Errors
/// Returns ``None`` if the captured piece's entity could not be found
/// Returns ``None`` if the file or rank could not be converted to isize (or back to usize after computation)
pub fn handle_en_passant(
    board: &mut Board,
    mut piece_move: PieceMove,
    moved_piece: Piece,
    mut piece_captured: bool,
    mut piece_moved_to: Piece,
) -> Option<(Option<TilePos>, PieceMove, bool, Piece)> {
    // Check if piece moved to the en passant tile
    if let Some(en_passant) = board.en_passant_on_last_move {
        // Moved to en passant tile and is the correct player's pawn
        if en_passant == piece_move.to
            && moved_piece == Piece::get_player_piece(board.get_player(), Piece::WPawn)
        {
            // Get the captured piece type from the Board
            let captured_piece_pos = TilePos::new(
                piece_move.to.file,
                piece_move.from.rank, // The rank which the piece moved from is the same as the piece it will capture
            );
            let captured_piece = board.positions.get_piece(captured_piece_pos);

            // Mark that there was a piece captured via en passant
            piece_captured = true;
            piece_move = piece_move.with_en_passant_capture();

            // Delete the piece at the captured tile
            board.positions.set_piece(captured_piece_pos, Piece::None);

            piece_moved_to = captured_piece;
        }
    }

    // Clear the en_passant marker, caching it for use in the history_move.make_move() function
    let en_passant_tile = board.en_passant_on_last_move;
    board.en_passant_on_last_move = None;

    // Check if this move allows en passant on the next move
    if Board::double_pawn_move_check(moved_piece, piece_move.from)
        && (isize::try_from(piece_move.from.rank).ok()?
            - isize::try_from(piece_move.to.rank).ok()?)
        .abs()
            == 2
    {
        let en_passant_tile = TilePos::new(
            piece_move.to.file,
            u32::try_from(
                isize::try_from(piece_move.from.rank).ok()? + Board::get_vertical_dir(moved_piece),
            )
            .ok()?,
        );

        board.en_passant_on_last_move = Some(en_passant_tile);
    }

    Some((en_passant_tile, piece_move, piece_captured, piece_moved_to))
}

#[allow(clippy::type_complexity)]
pub fn handle_castling(
    board: &mut Board,
    mut piece_move: PieceMove,
    moved_piece: Piece,
) -> Option<([(bool, bool); COLOUR_AMT], PieceMove, Option<bool>)> {
    // Remember the castling rights before this move
    let castling_rights_before_move = board.castling_rights;

    // Handle castling rights
    {
        let player_index = board.get_player().to_index();

        // Only update if the castling rights aren't already false
        if board.castling_rights[player_index] != (false, false) {
            // King was moved
            if moved_piece == board.positions.get_player_king(board.get_player()) {
                board.castling_rights[player_index] = (false, false);
            }
            // Rook was moved
            else if moved_piece == Piece::get_player_piece(board.get_player(), Piece::WRook) {
                // Kingside
                if piece_move.from.file == BOARD_SIZE - 1 {
                    board.castling_rights[player_index].0 = false;
                }
                // Queenside
                else if piece_move.from.file == 0 {
                    board.castling_rights[player_index].1 = false;
                }
            }
        }
    }

    let kingside_castle;
    (piece_move, kingside_castle) = perform_castling(board, piece_move, moved_piece, false)?;

    Some((castling_rights_before_move, piece_move, kingside_castle))
}

// Returns the piece_move and a boolean for if the castle was kingside
pub fn perform_castling(
    board: &mut Board,
    mut piece_move: PieceMove,
    moved_piece: Piece,
    undo: bool,
) -> Option<(PieceMove, Option<bool>)> {
    let mut kingside_castle = None;

    // If piece is this player's king, and the king moved 2 spaces
    let file_diff_isize =
        isize::try_from(piece_move.to.file).ok()? - isize::try_from(piece_move.from.file).ok()?;
    if moved_piece == board.positions.get_player_king(board.get_player())
        && file_diff_isize.unsigned_abs() == 2
        || undo
    {
        piece_move = piece_move.with_castling();

        // Kingside Castle
        if file_diff_isize > 0 {
            kingside_castle = Some(true);

            move_rook_for_castle(
                board,
                BOARD_SIZE - 1,
                BOARD_SIZE - 3,
                piece_move.from.rank,
                undo,
            );
        } else {
            kingside_castle = Some(false);

            move_rook_for_castle(board, 0, 3, piece_move.from.rank, undo);
        }
    }

    Some((piece_move, kingside_castle))
}

fn move_rook_for_castle(board: &mut Board, file: u32, new_file: u32, from_rank: u32, undo: bool) {
    let mut rook_pos = TilePos::new(file, from_rank);
    let mut new_rook_pos = TilePos::new(new_file, rook_pos.rank);

    if undo {
        std::mem::swap(&mut rook_pos, &mut new_rook_pos);
    }

    // Move the rook internally
    board
        .positions
        .move_piece(PieceMove::new(rook_pos, new_rook_pos));
}
