use std::fmt;

use crate::{
    bitboard::BitBoard,
    board::{BOARD_SIZE, PLAYERS, Player, TilePos},
    move_history::HistoryMove,
    piece::{COLOUR_AMT, PIECE_AMT, PIECES, Piece},
    piece_move::PieceMove,
};

#[derive(Default, Clone, Eq, PartialEq)]
pub struct BitBoards {
    pub boards: [BitBoard; PIECE_AMT * COLOUR_AMT],
    pub en_passant_tile: u64,
    pub castling_rights: [(bool, bool); COLOUR_AMT],
}

impl BitBoards {
    // TODO make const
    #[must_use]
    pub fn get_rank_mask(rank: u32) -> BitBoard {
        (0xFF << (rank * BOARD_SIZE)).into()
    }

    #[must_use]
    pub fn get_file_mask(file: u32) -> BitBoard {
        (0x0101_0101_0101_0101 << file).into()
    }

    #[must_use]
    pub fn get_piece(&self, tile_pos: TilePos) -> Piece {
        for &piece in PIECES {
            if self[piece].get_bit_at(tile_pos) {
                return piece;
            }
        }

        Piece::None
    }

    pub fn set_piece(&mut self, tile_pos: TilePos, piece: Piece) {
        for &piece_from_index in PIECES {
            if piece_from_index == piece {
                self[piece].set_bit_at(tile_pos, true);
            } else {
                self[piece_from_index].set_bit_at(tile_pos, false);
            }
        }
    }

    pub fn move_piece(&mut self, piece_move: PieceMove) {
        let piece = self.get_piece(piece_move.from);

        // Clear the original tile
        self[piece].set_bit_at(piece_move.from, false);

        // Clear the other boards at this space, and set the tile to this piece
        self.set_piece(piece_move.to, piece);
    }

    #[must_use]
    pub fn get_player_occupied(&self, player: Player) -> BitBoard {
        let mut occupied = 0.into();
        for (i, board) in self.boards.iter().enumerate() {
            // If the board is a piece for this player
            if PIECES[i].is_player(player) {
                occupied |= board.bits();
            }
        }

        occupied
    }

    #[must_use]
    pub fn get_occupied(&self) -> BitBoard {
        let mut occupied = 0.into();
        for board in self.boards {
            occupied |= board.bits();
        }

        occupied
    }

    // Apply and Undo Moves -------------------------------------------------------------------------------------------------------------------------

    /// # Panics
    /// Panics if player cannot be found from ``piece_move.from``
    pub fn apply_move(&mut self, piece_move: PieceMove) {
        let from_idx = piece_move.from.to_index();
        let to_idx = piece_move.to.to_index();

        let piece = self.get_piece(piece_move.from);
        let player = piece
            .to_player()
            .expect("Could not get Player from piece at piece_move.from"); // TODO

        // Clear the from position
        self[piece].set_bit(from_idx, false);

        // Handle captures
        let captured_piece = self.get_piece(piece_move.to);
        if captured_piece != Piece::None {
            self[captured_piece].set_bit(to_idx, false);
        }

        match piece_move.move_type {
            crate::piece_move::PieceMoveType::Normal => {
                // Set the moved to position to this piece
                self[piece].set_bit(to_idx, true);
            }
            crate::piece_move::PieceMoveType::EnPassant => {
                let captured_pos = TilePos::new(piece_move.to.file, piece_move.from.rank);
                let captured_idx = captured_pos.to_index();
                let captured_pawn = Piece::get_player_piece(player.next_player(), Piece::WPawn);

                // En passant capture the opposing pawn, Set the moved to position to this piece
                self[captured_pawn].set_bit(captured_idx, false);
                self[piece].set_bit(to_idx, true);
            }
            crate::piece_move::PieceMoveType::Castling => {
                // Get rook position depending on if this was kingside or queenside castle
                let (rook_from, rook_to) = if piece_move.to.file > piece_move.from.file {
                    (
                        TilePos::new(7, piece_move.from.rank),
                        TilePos::new(5, piece_move.from.rank),
                    )
                } else {
                    (
                        TilePos::new(0, piece_move.from.rank),
                        TilePos::new(3, piece_move.from.rank),
                    )
                };

                // Clear the rook from position and set its to position
                let rook = Piece::get_player_piece(player, Piece::WRook);
                self[rook].set_bit(rook_from.to_index(), false);
                self[rook].set_bit(rook_to.to_index(), true);

                // Set the moved to position to this piece
                self[piece].set_bit(to_idx, true);
            }
            crate::piece_move::PieceMoveType::Promotion(promoted_to) => {
                // let promoted_to = Piece::get_player_piece(player, promoted_to);

                // Set the moved to position to this piece
                self[promoted_to].set_bit(to_idx, true);
            }
        }

        // Update en passant square if needed
        if piece == Piece::get_player_piece(player, Piece::WPawn)
            && (piece_move.from.rank as i8 - piece_move.to.rank as i8).abs() == 2
        {
            self.en_passant_tile = 1
                << TilePos::new(
                    piece_move.from.file,
                    u32::midpoint(piece_move.from.rank, piece_move.to.rank),
                )
                .to_index();
        } else {
            self.en_passant_tile = 0;
        }
    }

    /// # Panics
    /// Panics if player cannot be found from ``piece_move.from``
    // TODO take reference of history
    pub fn undo_move(&mut self, history: HistoryMove) {
        let (piece_move, captured_piece, en_passant_tile, castling_rights) = history.into();

        let from_idx = piece_move.from.to_index();
        let to_idx = piece_move.to.to_index();

        let piece = self.get_piece(piece_move.from);
        let player = piece
            .to_player()
            .expect("Could not get Player from piece at piece_move.from"); // TODO

        // Clear the to position
        self[piece].set_bit(to_idx, false);

        match piece_move.move_type {
            crate::piece_move::PieceMoveType::Normal => self[piece].set_bit(from_idx, true),
            crate::piece_move::PieceMoveType::EnPassant => {
                self[piece].set_bit(from_idx, true);

                let captured_pos = TilePos::new(piece_move.to.file, piece_move.from.rank);
                let captured_idx = captured_pos.to_index();
                let captured_pawn = Piece::get_player_piece(player.next_player(), Piece::WPawn);

                self[captured_pawn].set_bit(captured_idx, true);
            }
            crate::piece_move::PieceMoveType::Castling => {
                self[piece].set_bit(from_idx, true);

                // Get rook position depending on if this was kingside or queenside castle
                let (rook_from, rook_to) = if piece_move.to.file > piece_move.from.file {
                    (
                        TilePos::new(7, piece_move.from.rank),
                        TilePos::new(5, piece_move.from.rank),
                    )
                } else {
                    (
                        TilePos::new(0, piece_move.from.rank),
                        TilePos::new(3, piece_move.from.rank),
                    )
                };

                // Clear the rook from position and set its to position
                let rook = Piece::get_player_piece(player, Piece::WRook);
                self[rook].set_bit(rook_to.to_index(), false);
                self[rook].set_bit(rook_from.to_index(), true);
            }
            crate::piece_move::PieceMoveType::Promotion(_) => {
                let pawn = Piece::get_player_piece(player, Piece::WPawn);
                self[pawn].set_bit(from_idx, true);
            }
        }

        if let Some(captured) = captured_piece {
            self[captured].set_bit(to_idx, true);
        }

        if let Some(en_passant_tile) = en_passant_tile {
            self.en_passant_tile = 1 << en_passant_tile.to_index();
        }

        self.castling_rights = castling_rights;
    }

    // END  ---- Apply and Undo Moves ----------------------------------------------------------------------------------------------------------------

    // Pseudolegal Move Generation -------------------------------------------------------------------------------------------------------------------

    /// # Panics
    /// Panics if the player cannot be found from the piece at from
    #[must_use]
    pub fn get_pawn_moves(&self, from: TilePos) -> BitBoard {
        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from");

        // TODO promotions

        let pawn = 1 << from.to_index();

        match player {
            Player::White => get_pawn_moves_for(
                pawn,
                self.get_player_occupied(Player::Black).bits(),
                !self.get_occupied().bits(),
                self.en_passant_tile,
                8,
                1,
                7,
                9,
                Self::get_file_mask(7).bits(),
                Self::get_file_mask(0).bits(),
            ),
            Player::Black => get_pawn_moves_for(
                pawn,
                self.get_player_occupied(Player::White).bits(),
                !self.get_occupied().bits(),
                self.en_passant_tile,
                -8,
                6,
                -9,
                -7,
                Self::get_file_mask(7).bits(),
                Self::get_file_mask(0).bits(),
            ),
        }
    }

    /// # Panics
    /// Panics if the player cannot be found from the piece at from
    #[must_use]
    pub fn get_knight_moves(&self, from: TilePos) -> BitBoard {
        let file_a: u64 = Self::get_file_mask(0).bits();
        let file_b: u64 = Self::get_file_mask(1).bits();
        let file_h: u64 = Self::get_file_mask(6).bits();
        let file_g: u64 = Self::get_file_mask(7).bits();

        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from"); // TODO Unwrap used

        let knight: BitBoard = (1 << from.to_index()).into();

        let mut moves: BitBoard = 0.into();

        // 2 up, 1 right (<< 17), no wrap if not on file A
        moves |= (knight << 17) & !file_a;

        // 2 up, 1 left (<< 15), no wrap if not on file H
        moves |= (knight << 15) & !file_h;

        // 1 up, 2 right (<< 10), no wrap if not on files A or B
        moves |= (knight << 10) & !(file_a | file_b);

        // 1 up, 2 left (<< 6), no wrap if not on files G or H
        moves |= (knight << 6) & !(file_g | file_h);

        // 2 down, 1 left (>> 17), no wrap if not on file H
        moves |= (knight >> 17) & !file_h;

        // 2 down, 1 right (>> 15), no wrap if not on file A
        moves |= (knight >> 15) & !file_a;

        // 1 down, 2 left (>> 10), no wrap if not on files G or H
        moves |= (knight >> 10) & !(file_g | file_h);

        // 1 down, 2 right (>> 6), no wrap if not on files A or B
        moves |= (knight >> 6) & !(file_a | file_b);

        // Remove squares occupied by own pieces
        moves & !self.get_player_occupied(player)
    }

    /// # Panics
    /// Panics if the player cannot be found from the piece at from
    #[must_use]
    pub fn get_king_moves(&self, from: TilePos) -> BitBoard {
        let file_a: u64 = Self::get_file_mask(0).bits();
        let file_h: u64 = Self::get_file_mask(7).bits();

        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from"); // TODO Unwrap used
        let king: BitBoard = (1 << from.to_index()).into();

        let mut moves: BitBoard = 0.into();

        // Up
        moves |= king << 8;
        // Down
        moves |= king >> 8;
        // Left
        moves |= (king >> 1) & !file_h;
        // Right
        moves |= (king << 1) & !file_a;
        // Up-Left
        moves |= (king << 7) & !file_h;
        // Up-Right
        moves |= (king << 9) & !file_a;
        // Down-Left
        moves |= (king >> 9) & !file_h;
        // Down-Right
        moves |= (king >> 7) & !file_a;

        // Remove own pieces squares
        moves & !self.get_player_occupied(player)
    }

    fn get_orthogonal_moves(&self, from: TilePos) -> BitBoard {
        let occupied = self.get_occupied().bits();

        let file_a: u64 = Self::get_file_mask(0).bits();
        let file_h: u64 = Self::get_file_mask(7).bits();

        let mut moves: BitBoard = 0.into();

        let mut temp: u64 = 1 << from.to_index();
        while temp != 0 {
            let square = temp.trailing_zeros();
            let from_bit = 1u64 << square;

            moves |= sliding_moves_in_direction(from_bit, occupied, 8, 0); // North
            moves |= sliding_moves_in_direction(from_bit, occupied, -8, 0); // South
            moves |= sliding_moves_in_direction(from_bit, occupied, 1, file_a); // East
            moves |= sliding_moves_in_direction(from_bit, occupied, -1, file_h); // West

            temp &= temp - 1; // Pop the LSB
        }

        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from");

        moves & !self.get_player_occupied(player)
    }

    #[must_use]
    pub fn get_rook_moves(&self, from: TilePos) -> BitBoard {
        self.get_orthogonal_moves(from)
    }

    fn get_diagonal_moves(&self, from: TilePos) -> BitBoard {
        let occupied = self.get_occupied().bits();

        let file_a: u64 = Self::get_file_mask(0).bits();
        let file_h: u64 = Self::get_file_mask(7).bits();

        let mut moves: BitBoard = 0.into();

        let mut temp: u64 = 1 << from.to_index();
        while temp != 0 {
            let square = temp.trailing_zeros();
            let from_bit = 1u64 << square;

            moves |= sliding_moves_in_direction(from_bit, occupied, 9, file_a); // NE
            moves |= sliding_moves_in_direction(from_bit, occupied, 7, file_h); // NW
            moves |= sliding_moves_in_direction(from_bit, occupied, -7, file_a); // SE
            moves |= sliding_moves_in_direction(from_bit, occupied, -9, file_h); // SW

            temp &= temp - 1;
        }

        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from");

        moves & !self.get_player_occupied(player)
    }

    #[must_use]
    pub fn get_bishop_moves(&self, from: TilePos) -> BitBoard {
        self.get_diagonal_moves(from)
    }

    #[must_use]
    pub fn get_queen_moves(&self, from: TilePos) -> BitBoard {
        self.get_orthogonal_moves(from) | self.get_diagonal_moves(from)
    }

    /// # Panics
    /// Panics if ``Piece::None`` is at from
    #[must_use]
    pub fn get_pseudolegal_moves(&self, from: TilePos) -> BitBoard {
        (match self.get_piece(from) {
            Piece::BPawn | Piece::WPawn => Self::get_pawn_moves,
            Piece::BKnight | Piece::WKnight => Self::get_knight_moves,
            Piece::BKing | Piece::WKing => Self::get_king_moves,
            Piece::BRook | Piece::WRook => Self::get_rook_moves,
            Piece::BBishop | Piece::WBishop => Self::get_bishop_moves,
            Piece::BQueen | Piece::WQueen => Self::get_queen_moves,
            Piece::None => {
                panic!("Tried to get pseudolegal moves of Piece::None");
            }
        })(self, from)
    }

    // END ---- Pseudolegal Move Generation ----------------------------------------------------------------------------------------------------------

    #[must_use]
    pub fn get_all_player_pieces(&self, player: Player) -> Vec<(Piece, TilePos)> {
        self.get_player_occupied(player)
            .to_tile_positions()
            .iter()
            .filter_map(|&pos| {
                let piece = self.get_piece(pos);

                if piece.to_player() == Some(player) {
                    Some((piece, pos))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    #[must_use]
    pub fn get_attacked_tiles(&self, player: Player) -> BitBoard {
        let mut attacked = 0;

        for (i, &board) in self.boards.iter().enumerate() {
            if PIECES[i].is_player(player) {
                continue;
            }

            let mut bits = board.bits();
            while bits != 0 {
                let from = bits.trailing_zeros();

                bits &= bits - 1; // Clear LSB

                attacked |= self.get_pseudolegal_moves(TilePos::from_index(from)).bits();
            }
        }

        attacked.into()
    }

    #[must_use]
    pub fn is_pos_attacked(&self, pos: TilePos) -> bool {
        (self.get_piece(pos).to_player().map_or_else(
            || self.get_attacked_tiles(Player::White) | self.get_attacked_tiles(Player::Black),
            |player| self.get_attacked_tiles(player.next_player()),
        ) & (1 << pos.to_index()))
        .bits()
            != 0
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

    /// # Panics
    /// Panics if the player cannot be found from the piece at from
    #[must_use]
    pub fn get_possible_moves(&self, from: TilePos) -> Vec<PieceMove> {
        let mut moves = Vec::new();

        let mut bits = self.get_pseudolegal_moves(from).bits();
        while bits != 0 {
            let to = TilePos::from_index(bits.trailing_zeros());

            // Clear LSB
            bits &= bits - 1;

            // Add the move if it  doesn't make this player's king become attacked
            let new_move = PieceMove::new(from, to);
            let player = self
                .get_piece(from)
                .to_player()
                .expect("Could not get player from Piece at from"); // TODO
            if !self.move_makes_pos_attacked(new_move, self.get_king_pos(player)) {
                moves.push(new_move);
            }
        }

        moves
    }

    #[must_use]
    pub fn get_all_possible_moves(&self, player: Player) -> Vec<PieceMove> {
        let mut moves = Vec::new();

        for (i, &board) in self.boards.iter().enumerate() {
            if !PIECES[i].is_player(player) {
                continue;
            }

            let mut bits = board.bits();
            while bits != 0 {
                let from = TilePos::from_index(bits.trailing_zeros());

                // Clear LSB
                bits &= bits - 1;

                let mut possible_moves = self.get_possible_moves(from);
                moves.append(&mut possible_moves);
            }
        }

        moves
    }

    #[must_use]
    pub const fn get_player_king(&self, player: Player) -> Piece {
        match player {
            Player::White => Piece::WKing,
            Player::Black => Piece::BKing,
        }
    }

    #[must_use]
    pub fn get_king_pos(&self, player: Player) -> TilePos {
        self[self.get_player_king(player)].to_tile_positions()[0] // Should always have a king
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
                .get_player_occupied(player)
                .to_tile_positions()
                .iter()
                .flat_map(|&piece_pos| self.get_possible_moves(piece_pos))
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

impl fmt::Display for BitBoards {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut message = String::new();

        for rank in (0..BOARD_SIZE).rev() {
            for file in 0..BOARD_SIZE {
                let piece = {
                    let found_pieces = self
                        .boards
                        .iter()
                        .zip(PIECES)
                        .filter_map(|(board, &piece)| {
                            if board.get_bit_at(TilePos::new(file, rank)) {
                                Some(piece)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    if found_pieces.is_empty() {
                        Piece::None
                    } else {
                        // Should only ever have one piece on each type
                        found_pieces[0]
                    }
                };

                let piece_char = Into::<char>::into(piece);

                message += format!("{piece_char} ").as_str();
            }

            if rank > 0 {
                message.push('\n');
            }
        }

        write!(f, "{message}")
    }
}

impl std::ops::Index<Piece> for BitBoards {
    type Output = BitBoard;

    fn index(&self, piece: Piece) -> &Self::Output {
        match piece {
            Piece::None => panic!("Tried to use Piece::None as an index"),
            _ => &self.boards[piece.to_bitboard_index()],
        }
    }
}

impl std::ops::IndexMut<Piece> for BitBoards {
    fn index_mut(&mut self, piece: Piece) -> &mut Self::Output {
        match piece {
            Piece::None => panic!("Tried to use Piece::None as an index"),
            _ => &mut self.boards[piece.to_bitboard_index()],
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn get_pawn_moves_for(
    pawns: u64,
    opposing_pieces: u64,
    empty: u64,
    en_passant_tile: u64,
    forward: i8,
    start_rank: u32,
    left_shift: i8,
    right_shift: i8,
    left_file_mask: u64,
    right_file_mask: u64,
) -> BitBoard {
    // Pushing
    let single_push = shift_i8(pawns, forward) & empty;
    let double_push = shift_i8(
        shift_i8(pawns & BitBoards::get_rank_mask(start_rank).bits(), forward) & empty,
        forward,
    ) & empty;

    // Capturing
    let left_capture = shift_i8(pawns, left_shift) & opposing_pieces & !right_file_mask;
    let right_capture = shift_i8(pawns, right_shift) & opposing_pieces & !left_file_mask;

    // En Passant Capturing
    let en_passant_capture_left = shift_i8(pawns, left_shift) & en_passant_tile & !right_file_mask;
    let en_passant_capture_right = shift_i8(pawns, right_shift) & en_passant_tile & !left_file_mask;

    (single_push
        | double_push
        | left_capture
        | right_capture
        | en_passant_capture_left
        | en_passant_capture_right)
        .into()
}

const fn sliding_moves_in_direction(
    mut position: u64,
    occupied: u64,
    shift_amt: i8,
    edge_mask: u64,
) -> u64 {
    let mut moves = 0;
    loop {
        // Shift position by one step
        position = shift_i8(position, shift_amt) & !edge_mask;

        if position == 0 {
            break;
        }

        moves |= position;

        // If the position intersects occupied squares, stop sliding in this direction
        if (position & occupied) != 0 {
            break;
        }
    }
    moves
}

const fn shift_i8(bits: u64, shift_amt: i8) -> u64 {
    if shift_amt > 0 {
        bits << shift_amt
    } else {
        bits >> shift_amt.abs()
    }
}
