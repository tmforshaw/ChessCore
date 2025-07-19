use std::fmt;

use crate::{
    bitboard::BitBoards,
    move_history::PieceMoveHistory,
    piece::{COLOUR_AMT, PIECES, Piece},
    piece_move::{
        PieceMove, PieceMoveType, apply_promotion, handle_castling, handle_en_passant,
        perform_castling, perform_promotion,
    },
    possible_moves::{get_possible_moves, get_pseudolegal_moves},
};

pub const BOARD_SIZE: usize = 8;

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
    pub file: usize,
    pub rank: usize,
}

impl TilePos {
    #[must_use]
    pub const fn new(file: usize, rank: usize) -> Self {
        Self { file, rank }
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

impl From<(usize, usize)> for TilePos {
    fn from((file, rank): (usize, usize)) -> Self {
        Self::new(file, rank)
    }
}

impl From<TilePos> for (usize, usize) {
    fn from(value: TilePos) -> Self {
        (value.file, value.rank)
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Board {
    pub positions: BitBoards,
    pub player: Player,
    pub castling_rights: [(bool, bool); COLOUR_AMT],
    pub en_passant_on_last_move: Option<TilePos>,
    pub half_move_counter: usize,
    pub full_move_counter: usize,
    pub move_history: PieceMoveHistory,
}

impl Default for Board {
    fn default() -> Self {
        const DEFAULT_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; // Normal Starting Board

        // const DEFAULT_FEN: &str = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"; // Castling Test Board

        // const DEFAULT_FEN: &str = "rnbqkbnr/p1p1pppp/1p6/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"; // En Pasasnt Test Board

        // const DEFAULT_FEN: &str =
        //     "rnbqkbnr/1ppp1ppp/8/p3p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 4"; // Scholar's Mate Board

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
            castling_rights: [(false, false); COLOUR_AMT],
            en_passant_on_last_move: None,
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
                    '1'..='8' => file += (chr as u8 - b'0') as usize,
                    ' ' => section_index += 1,
                    _ => {
                        if let Some(piece) = Piece::from_algebraic(chr) {
                            let tile_pos = TilePos::new(file, BOARD_SIZE - 1 - rank); // Count from the bottom (need to flip rank)
                            board.set_piece(tile_pos, piece);
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
                    'K' => board.castling_rights[Player::White as usize].0 = true,
                    'Q' => board.castling_rights[Player::White as usize].1 = true,
                    'k' => board.castling_rights[Player::Black as usize].0 = true,
                    'q' => board.castling_rights[Player::Black as usize].1 = true,
                    '-' => board.castling_rights = [(false, false); COLOUR_AMT],
                    ' ' => section_index += 1,
                    _ => {
                        return Err(format!(
                            "Could not create board using FEN string [{fen}]:\n'{chr}' does not provide valid castling rights information"
                        ));
                    }
                },
                // Reached the en passant part of FEN
                3 => match chr {
                    '-' => board.en_passant_on_last_move = None,
                    ' ' => section_index += 1,
                    c => {
                        if !c.is_ascii_digit() {
                            let algebraic_en_passant =
                                fen.chars().skip(chr_index).take(2).collect::<Vec<_>>();

                            match (algebraic_en_passant[0], algebraic_en_passant[1]) {
                                ('a'..='h', '0'..='8') => {
                                    board.en_passant_on_last_move = Some(TilePos::new(
                                        (algebraic_en_passant[0] as u8 - b'a') as usize,
                                        (algebraic_en_passant[1] as u8 - b'1') as usize,
                                    ));
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
    pub fn apply_move(&mut self, mut piece_move: PieceMove) {
        // TODO Duplicated Code

        let mut piece_captured = false;

        // Capture any pieces that should be captured
        let mut piece_moved_to = if self.get_piece(piece_move.to) == Piece::None {
            Piece::None
        } else {
            piece_captured = true;

            self.get_piece(piece_move.to)
        };

        let moved_piece = self.get_piece(piece_move.from);

        // Handle promotion
        piece_move = apply_promotion(self, moved_piece, piece_move);

        // Handle en passant, if this move is en passant, or if this move allows en passant on the next move
        let en_passant_tile;
        (en_passant_tile, piece_move, piece_captured, piece_moved_to) = handle_en_passant(
            self,
            piece_move,
            moved_piece,
            piece_captured,
            piece_moved_to,
        )
        .expect("Could not handle en passant in apply_move");

        // Handle Castling
        let castling_rights_before_move;
        (castling_rights_before_move, piece_move, _) =
            handle_castling(self, piece_move, moved_piece)
                .expect("Castling could not be handled in apply_move");

        // Move the piece internally and update its entity translation
        self.move_piece(piece_move);

        let captured_piece = if piece_captured {
            Some(piece_moved_to)
        } else {
            None
        };

        // Update the move history with this move
        self.move_history.make_move(
            piece_move,
            captured_piece,
            en_passant_tile,
            castling_rights_before_move,
        );

        self.next_player();
    }

    /// # Panics
    /// Panics if castling cannot be undone, or if piece couldn't un-promote
    pub fn undo_move(&mut self) {
        // TODO Duplicated Code

        // Check if this move caused the game to end
        let game_didnt_end = self.has_game_ended().is_none();

        let Some(history_move) = self.move_history.traverse_prev() else {
            return;
        };

        let (mut piece_move, captured_piece, en_passant_tile, castling_rights) =
            history_move.into();

        // Set the castling rights
        self.castling_rights = castling_rights;

        // Set the en_passant marker
        self.en_passant_on_last_move = en_passant_tile;

        // Perform the correct move for the move_type
        match piece_move.move_type {
            PieceMoveType::Castling => {
                // Perform the castling
                let moved_piece = self.get_piece(piece_move.to);

                (piece_move, _) = perform_castling(self, piece_move, moved_piece, true)
                    .expect("Castling couldn't be undone");
            }
            PieceMoveType::Promotion(_) => {
                // Get the piece's player as an index
                let player_index = self
                    .get_piece(piece_move.to)
                    .to_player()
                    .expect("Player could not be found via piece move for promotion")
                    .to_index();

                // Get this player's pawn type
                let new_piece_type = self.get_player_piece(PLAYERS[player_index], Piece::WPawn);

                perform_promotion(self, piece_move.to, new_piece_type);
            }
            _ => {}
        }

        // Move piece before re-creating captured pieces
        self.move_piece(piece_move.rev());

        match piece_move.move_type {
            PieceMoveType::Normal | PieceMoveType::EnPassant => {
                // TODO
                // Create new entities for any captured pieces
                if let Some(captured_piece) = captured_piece {
                    // Set the captured piece tile, depending on if this capture was an en passant capture or not
                    let captured_piece_tile = if piece_move.move_type == PieceMoveType::EnPassant {
                        TilePos::new(piece_move.to.file, piece_move.from.rank)
                    } else {
                        piece_move.to
                    };

                    // Update the board to make it aware of the spawned piece
                    self.set_piece(captured_piece_tile, captured_piece);
                }
            }
            _ => {}
        }

        // Only increment the player if the game didn't end on this move
        if game_didnt_end {
            self.next_player();
        }
    }

    pub fn move_piece(&mut self, piece_move: PieceMove) {
        let moved_piece = self.get_piece(piece_move.from);
        self.set_piece(piece_move.from, Piece::None);
        self.set_piece(piece_move.to, moved_piece);
    }

    #[must_use]
    pub fn get_piece(&self, tile_pos: TilePos) -> Piece {
        for &piece in PIECES {
            if self.positions[piece].get_bit_at(tile_pos) {
                return piece;
            }
        }

        Piece::None
    }

    pub fn set_piece(&mut self, tile_pos: TilePos, piece: Piece) {
        // Clear all the other bitboards at this position, except this piece's position bitboard
        for &piece_i in PIECES {
            if piece_i == piece {
                self.positions[piece_i].set_bit_at(tile_pos, true);
            } else {
                self.positions[piece_i].set_bit_at(tile_pos, false);
            }
        }
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
    pub fn get_all_player_piece_pos(&self, player: Player) -> Vec<TilePos> {
        self.get_all_player_pieces(player)
            .into_iter()
            .map(|(_, pos)| pos)
            .collect::<Vec<_>>()
    }

    #[must_use]
    pub fn get_all_player_pieces(&self, player: Player) -> Vec<(Piece, TilePos)> {
        self.positions
            .boards
            .iter()
            .enumerate()
            .filter_map(|(i, &board)| {
                if Piece::from(i).to_player() == Some(player) {
                    Some((Piece::from(i), board.get_positions()))
                } else {
                    None
                }
            })
            .flat_map(|(piece, positions)| positions.into_iter().map(move |pos| (piece, pos)))
            .collect::<Vec<_>>()
    }

    #[must_use]
    fn get_moves_in_dir(&self, from: TilePos, dirs: Vec<(isize, isize)>) -> Option<Vec<TilePos>> {
        let mut positions = Vec::new();

        let board_size_isize = isize::try_from(BOARD_SIZE).ok()?;

        for dir in dirs {
            for k in 1..(board_size_isize) {
                let new_file = isize::try_from(from.file).ok()? + dir.0 * k;
                let new_rank = isize::try_from(from.rank).ok()? + dir.1 * k;

                // New pos is within the board
                if new_file >= 0
                    && new_file < board_size_isize
                    && new_rank >= 0
                    && new_rank < board_size_isize
                {
                    let new_pos = TilePos::new(
                        usize::try_from(new_file).ok()?,
                        usize::try_from(new_rank).ok()?,
                    );

                    let piece = self.get_piece(from);
                    let captured_piece = self.get_piece(new_pos);
                    if captured_piece != Piece::None {
                        if captured_piece.to_player() != piece.to_player() {
                            positions.push(new_pos);
                        }

                        break;
                    }

                    positions.push(new_pos);
                }
            }
        }

        Some(positions)
    }

    #[must_use]
    pub fn get_orthogonal_moves(&self, from: TilePos) -> Option<Vec<TilePos>> {
        self.get_moves_in_dir(from, vec![(1, 0), (0, 1), (-1, 0), (0, -1)])
    }

    #[must_use]
    pub fn get_diagonal_moves(&self, from: TilePos) -> Option<Vec<TilePos>> {
        self.get_moves_in_dir(from, vec![(1, 1), (1, -1), (-1, 1), (-1, -1)])
    }

    #[must_use]
    pub fn get_ortho_diagonal_moves(&self, from: TilePos) -> Option<Vec<TilePos>> {
        let mut positions = self.get_orthogonal_moves(from)?;
        positions.append(&mut self.get_diagonal_moves(from)?);

        Some(positions)
    }

    fn get_castling_pos(&self, from: TilePos, file: usize) -> Option<TilePos> {
        // Get Rook Position
        let rook = TilePos::new(file, from.rank);

        // Check that it is empty between the rook and the king
        if self.is_empty_between(from, rook) {
            // Check that there are no attacked tiles between the rook and the king
            let tiles_between = self.get_tiles_between(from, rook)?;

            let mut attacked_between = false;
            for tile in tiles_between {
                if self.is_pos_attacked(tile) {
                    attacked_between = true;
                    break;
                }
            }

            if !attacked_between {
                let new_file = if from.file > file {
                    from.file - 2
                } else {
                    from.file + 2
                };
                return Some(TilePos::new(new_file, from.rank));
            }
        }

        None
    }

    #[must_use]
    pub fn get_knight_moves(&self, from: TilePos) -> Option<Vec<TilePos>> {
        let mut positions = Vec::new();

        let file_isize = isize::try_from(from.file).ok()?;
        let rank_isize = isize::try_from(from.rank).ok()?;
        let board_size_isize = isize::try_from(BOARD_SIZE).ok()?;

        for i in [-2, -1, 1, 2_isize] {
            for j in [-2, -1, 1, 2_isize] {
                if i.abs() != j.abs()
                    && file_isize + i >= 0
                    && file_isize + i < board_size_isize
                    && rank_isize + j >= 0
                    && rank_isize + j < board_size_isize
                {
                    let new_pos = TilePos::new(
                        usize::try_from(file_isize + i).ok()?,
                        usize::try_from(rank_isize + j).ok()?,
                    );

                    let captured_piece = self.get_piece(new_pos);
                    if captured_piece.to_player() != self.get_piece(from).to_player()
                        || captured_piece == Piece::None
                    {
                        positions.push(new_pos);
                    }
                }
            }
        }

        Some(positions)
    }

    #[must_use]
    pub fn get_king_moves(&self, from: TilePos) -> Option<Vec<TilePos>> {
        let mut positions = Vec::new();

        let file_isize = isize::try_from(from.file).ok()?;
        let rank_isize = isize::try_from(from.rank).ok()?;
        let board_size_isize = isize::try_from(BOARD_SIZE).ok()?;

        let player = self.get_piece(from).to_player();

        // Normal movement
        for i in [-1, 0, 1] {
            for j in [-1, 0, 1] {
                if !(i == 0 && j == 0) {
                    let vertical = file_isize + i;
                    let horizontal = rank_isize + j;

                    if vertical >= 0
                        && vertical < board_size_isize
                        && horizontal >= 0
                        && horizontal < board_size_isize
                    {
                        let new_pos = TilePos::new(
                            usize::try_from(file_isize + i).ok()?,
                            usize::try_from(rank_isize + j).ok()?,
                        );

                        if self.get_piece(new_pos).to_player() != player {
                            positions.push(new_pos);
                        }
                    }
                }
            }
        }

        // Castling
        if let Some(player) = player {
            let player_index = player.to_index();

            // Kingside Castling
            if self.castling_rights[player_index].0 {
                if let Some(pos) = self.get_castling_pos(from, BOARD_SIZE - 1) {
                    positions.push(pos);
                }
            }

            // Queenside Castling
            if self.castling_rights[player_index].1 {
                if let Some(pos) = self.get_castling_pos(from, 0) {
                    positions.push(pos);
                }
            }
        }

        Some(positions)
    }

    #[must_use]
    pub fn get_pawn_moves(&self, from: TilePos) -> Option<Vec<TilePos>> {
        let piece = self.get_piece(from);
        let vertical_dir = Self::get_vertical_dir(piece);

        let file_isize = isize::try_from(from.file).ok()?;
        let rank_isize = isize::try_from(from.rank).ok()?;
        let board_size_isize = isize::try_from(BOARD_SIZE).ok()?;

        let mut positions = Vec::new();

        // Single Move Vertically and Diagonal Captures
        let new_vertical_pos = rank_isize + vertical_dir;
        if new_vertical_pos >= 0 && new_vertical_pos < board_size_isize {
            // Single Move Vertically
            let new_pos = TilePos::new(from.file, usize::try_from(rank_isize + vertical_dir).ok()?);
            if self.get_piece(new_pos) == Piece::None {
                positions.push(new_pos);
            }

            // Diagonal Captures
            for k in [-1, 1] {
                let new_horizontal_pos = file_isize + k;

                if new_horizontal_pos >= 0 && new_horizontal_pos < board_size_isize {
                    if let Some(player) = piece.to_player() {
                        let new_pos = TilePos::new(
                            usize::try_from(new_horizontal_pos).ok()?,
                            usize::try_from(new_vertical_pos).ok()?,
                        );

                        if let Some(captured_player) = self.get_piece(new_pos).to_player() {
                            if player != captured_player {
                                positions.push(new_pos);
                            }
                        }
                    }
                }
            }
        }

        // En passant
        if let Some(passant_tile) = self.en_passant_on_last_move {
            let file_diff = isize::try_from(passant_tile.file).ok()? - file_isize;
            let rank_diff = isize::try_from(passant_tile.rank).ok()? - rank_isize;

            // Is able to take the en passant square
            if file_diff.abs() == 1 && rank_diff == vertical_dir {
                positions.push(passant_tile);
            }
        }

        // Double Vertical Move
        if Self::double_pawn_move_check(piece, from) {
            let new_pos = TilePos::new(
                from.file,
                usize::try_from(rank_isize + 2 * vertical_dir).ok()?,
            );

            // Is empty between these points, and there is nothing at that tile
            if self.is_empty_between(from, new_pos) && self.get_piece(new_pos) == Piece::None {
                positions.push(new_pos);
            }
        }

        Some(positions)
    }

    // Get the tiles which are attacked by the opposing player
    #[must_use]
    pub fn get_attacked_tiles(&self, player: Player) -> Vec<TilePos> {
        self.positions
            .boards
            .iter()
            .enumerate()
            .filter_map(|(i, &board)| {
                // Choose only the boards for pieces which are not this player's
                if PIECES[i].is_player(player) {
                    None
                } else {
                    Some(board)
                }
            })
            .flat_map(|board| {
                // Get the pseudolegal moves for all pieces of this type
                board
                    .get_positions()
                    .iter()
                    .filter_map(|&pos| get_pseudolegal_moves(self, pos))
                    .flat_map(IntoIterator::into_iter)
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>()
    }

    #[must_use]
    pub fn is_pos_attacked(&self, pos: TilePos) -> bool {
        self.get_piece(pos)
            .to_player()
            .is_some_and(|player| self.get_attacked_tiles(player).contains(&pos))
    }

    #[must_use]
    pub fn move_makes_pos_attacked(&self, piece_move: PieceMove, pos: TilePos) -> bool {
        // Move the piece on a cloned board
        let mut test_board = self.clone();
        test_board.move_piece(piece_move);

        // Check if the tile which we are testing is the piece which is being moved
        let pos = if pos == piece_move.from {
            // Move the tile which is being tested to this new position
            piece_move.to
        } else {
            pos
        };

        test_board.is_pos_attacked(pos)
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

        let file_diff = usize::from(file_diff_isize != 0);
        let rank_diff = usize::from(rank_diff_isize != 0);

        Some(
            (1..((file_diff_isize.unsigned_abs()).max(rank_diff_isize.unsigned_abs())))
                .map(|k| {
                    TilePos::new(
                        lower_pos.file + k * file_diff,
                        lower_pos.rank + k * rank_diff,
                    )
                })
                .collect::<Vec<_>>(),
        )
    }

    #[must_use]
    pub fn is_empty_between(&self, pos1: TilePos, pos2: TilePos) -> bool {
        let Some(tiles_between) = self.get_tiles_between(pos1, pos2) else {
            return false;
        };

        for tile in tiles_between {
            if self.get_piece(tile) != Piece::None {
                return false;
            }
        }

        true
    }

    #[must_use]
    pub const fn get_player_king(&self, player: Player) -> Piece {
        match player {
            Player::White => Piece::WKing,
            Player::Black => Piece::BKing,
        }
    }

    #[must_use]
    pub const fn get_player_piece(&self, player: Player, piece: Piece) -> Piece {
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

    #[must_use]
    pub fn get_king_pos(&self, player: Player) -> TilePos {
        self.positions[self.get_player_king(player)].get_positions()[0] // Should always have a king
    }

    #[must_use]
    pub fn get_all_possible_moves(&self, player: Player) -> Vec<PieceMove> {
        self.positions
            .boards
            .into_iter()
            .enumerate()
            .filter_map(|(i, bitboard)| {
                if Piece::from(i).is_player(player) {
                    Some(
                        bitboard
                            .get_positions()
                            .into_iter()
                            .filter_map(|from_pos| {
                                get_possible_moves(self, from_pos)
                                    .map(|positions| (from_pos, positions))
                            })
                            .flat_map(|(from_pos, positions)| {
                                positions
                                    .into_iter()
                                    .map(move |to_pos| PieceMove::new(from_pos, to_pos))
                            }),
                    )
                } else {
                    None
                }
            })
            .flat_map(IntoIterator::into_iter)
            .collect::<Vec<_>>()
    }

    #[must_use]
    pub fn has_game_ended(&self) -> Option<Option<Player>> {
        // Get the position of all kings
        for (player, king_pos) in PLAYERS
            .iter()
            .map(|&player| (player, self.get_king_pos(player)))
        {
            // No moves for this player
            if self
                .get_all_player_piece_pos(player)
                .iter()
                .filter_map(|&piece_pos| get_possible_moves(self, piece_pos))
                .flat_map(IntoIterator::into_iter)
                .next()
                .is_none()
            {
                // King is in check, it is checkmate
                if self.is_pos_attacked(king_pos) {
                    let opposite_player = player.next_player();
                    return Some(Some(opposite_player));
                }

                // Stalemate
                return Some(None);
            }
        }

        None
    }
}
