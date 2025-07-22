use std::fmt;

use crate::{
    bitboard::BitBoard,
    move_history::{HistoryMove, PieceMoveHistory},
    piece::{COLOUR_AMT, PIECE_AMT, PIECES, Piece},
    piece_move::{PieceMove, PieceMoveType},
};

pub const BOARD_SIZE: u32 = 8;

pub const NORMAL_START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; // Normal Starting Board
pub const CASTLING_FEN: &str = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"; // Castling Test Board
pub const EN_PASSANT_FEN: &str = "rnbqkbnr/p1p1pppp/1p6/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"; // En Pasasnt Test Board
pub const SCHOLARS_MATE_FEN: &str = "rnbqkbnr/1ppp1ppp/8/p3p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 4"; // Scholar's Mate Board
pub const PROMOTION_FEN: &str = "8/1ppkp1P1/3pp3/8/8/5PP1/p2PPKP1/8 w - - 1 1"; // Promotion Test Board
pub const PROMOTION_CAPTURE_FEN: &str = "rn1qk1nr/pPppppPp/8/8/8/8/PpPPPPpP/RN1QK1NR w KQkq - 0 1"; // Capture Promotion Corner
pub const STALEMATE_FEN: &str = "rnbqkbnr/pppppppp/8/8/6r1/7p/7P/7K b - - 1 1"; // Stalemate Test Board

pub const DEFAULT_FEN: &str = SCHOLARS_MATE_FEN;

#[derive(Clone, Eq, PartialEq)]
pub struct Board {
    pub boards: [BitBoard; PIECE_AMT * COLOUR_AMT],
    pub en_passant_tile: u64,
    pub castling_rights: [(bool, bool); COLOUR_AMT],
    pub player: Player,
    pub half_move_counter: usize,
    pub full_move_counter: usize,
    pub move_history: PieceMoveHistory,
}

impl Default for Board {
    fn default() -> Self {
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
            boards: [BitBoard::default(); PIECE_AMT * COLOUR_AMT],
            en_passant_tile: 0,
            castling_rights: [(true, true); COLOUR_AMT],
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
                            board.set_piece(tile_pos, piece);
                            board[piece].set_bit_at(tile_pos, true);

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
                    '-' => board.en_passant_tile = 0,
                    ' ' => section_index += 1,
                    c => {
                        if !c.is_ascii_digit() {
                            let algebraic_en_passant = fen.chars().skip(chr_index).take(2).collect::<Vec<_>>();

                            match (algebraic_en_passant[0], algebraic_en_passant[1]) {
                                ('a'..='h', '0'..='8') => {
                                    board.en_passant_tile = 1
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
    pub fn apply_move(&mut self, mut piece_move: PieceMove) -> PieceMoveType {
        let from_idx = piece_move.from.to_index();
        let to_idx = piece_move.to.to_index();

        let piece = self.get_piece(piece_move.from);
        let player = piece.to_player().expect("Could not get Player from piece at piece_move.from");

        // Classify the move_type of this piece_move

        // Castling Move
        if piece == Piece::get_player_piece(player, Piece::WKing)
            && (piece_move.from.rank as i8 - piece_move.to.rank as i8) == 0
            && (piece_move.from.file as i8 - piece_move.to.file as i8).unsigned_abs() == 2
        {
            piece_move.give_castling();
        } else if piece == Piece::get_player_piece(player, Piece::WPawn) {
            // Was an en passant capture
            if piece_move.to.to_index() == self.en_passant_tile.trailing_zeros() {
                piece_move.give_en_passant_capture();
            }
            // Promotion
            else if (piece_move.to.rank == BOARD_SIZE - 1 && player == Player::White)
                || (piece_move.to.rank == 0 && player == Player::Black)
            {
                // TODO decide which piece to promote to

                let promoted_to = Piece::get_player_piece(player, Piece::WQueen);
                piece_move.give_promotion(promoted_to);
            }
        }

        // Clear the from position
        self[piece].set_bit(from_idx, false);

        // Handle captures
        let mut captured_piece = self.get_piece(piece_move.to);
        if captured_piece != Piece::None {
            self[captured_piece].set_bit(to_idx, false);
        }

        // Remember the castling rights before the move was made
        let castling_rights_before = self.castling_rights;

        // Might need to update castling rights
        if piece == Piece::get_player_piece(player, Piece::WKing) || piece == Piece::get_player_piece(player, Piece::WRook) {
            let castling_rights = match player {
                Player::White => &mut self.castling_rights[0],
                Player::Black => &mut self.castling_rights[1],
            };

            // King was moved
            if piece == Piece::get_player_piece(player, Piece::WKing) {
                castling_rights.0 = false;
                castling_rights.1 = false;
            } else {
                // Kingside Move
                if piece_move.from.file == 7 {
                    castling_rights.0 = false;
                }
                // Queenside Move
                else {
                    castling_rights.1 = false;
                }
            }
        }

        match piece_move.move_type {
            PieceMoveType::Normal => {
                // Set the moved to position to this piece
                self[piece].set_bit(to_idx, true);
            }
            PieceMoveType::EnPassant => {
                let captured_pos = TilePos::new(piece_move.to.file, piece_move.from.rank);
                let captured_idx = captured_pos.to_index();
                let captured_pawn = Piece::get_player_piece(player.next_player(), Piece::WPawn);

                // En passant capture the opposing pawn, Set the moved to position to this piece
                self[captured_pawn].set_bit(captured_idx, false);
                self[piece].set_bit(to_idx, true);

                // Set the captured_piece so it is remembered by move history
                captured_piece = captured_pawn;
            }
            PieceMoveType::Castling => {
                // Get rook position depending on if this was kingside or queenside castle
                let (rook_from, rook_to) = if piece_move.to.file > piece_move.from.file {
                    (TilePos::new(7, piece_move.from.rank), TilePos::new(5, piece_move.from.rank))
                } else {
                    (TilePos::new(0, piece_move.from.rank), TilePos::new(3, piece_move.from.rank))
                };

                // Clear the rook from position and set its to position
                let rook = Piece::get_player_piece(player, Piece::WRook);
                self[rook].set_bit(rook_from.to_index(), false);
                self[rook].set_bit(rook_to.to_index(), true);

                // Set the moved to position to this piece
                self[piece].set_bit(to_idx, true);
            }
            PieceMoveType::Promotion(promoted_to) => {
                // Set the moved to position to this piece
                self[promoted_to].set_bit(to_idx, true);
            }
        }

        // Remember the en_passant tile for move_history
        let en_passant_before = if self.en_passant_tile == 0 {
            None
        } else {
            Some(TilePos::from_index(self.en_passant_tile.trailing_zeros()))
        };

        // Update en passant square if needed
        if piece == Piece::get_player_piece(player, Piece::WPawn)
            && (piece_move.from.rank as i8 - piece_move.to.rank as i8).abs() == 2
        {
            self.en_passant_tile =
                1 << TilePos::new(piece_move.from.file, u32::midpoint(piece_move.from.rank, piece_move.to.rank)).to_index();
        } else {
            self.en_passant_tile = 0;
        }
        // Update the move history with this move
        self.move_history.make_move(
            piece_move,
            if captured_piece == Piece::None {
                None
            } else {
                Some(captured_piece)
            },
            en_passant_before,
            castling_rights_before,
        );

        self.next_player();

        piece_move.move_type
    }

    /// # Panics
    /// Panics if player cannot be found from ``piece_move.from``
    pub fn undo_move(&mut self, history_move: HistoryMove) -> Option<HistoryMove> {
        let (piece_move, captured_piece, en_passant_tile, castling_rights) = history_move.into();

        // Make sure the piece_move won't end up in the move history
        let piece_move = piece_move.with_show(false);

        let from_idx = piece_move.from.to_index();
        let to_idx = piece_move.to.to_index();

        let piece = self.get_piece(piece_move.to);
        let player = piece.to_player().expect("Could not get Player from piece at piece_move.from");

        // Clear the to position
        self[piece].set_bit(to_idx, false);

        match piece_move.move_type {
            PieceMoveType::Normal => self[piece].set_bit(from_idx, true),
            PieceMoveType::EnPassant => {
                self[piece].set_bit(from_idx, true);

                let captured_pos = TilePos::new(piece_move.to.file, piece_move.from.rank);
                let captured_idx = captured_pos.to_index();
                let captured_pawn = Piece::get_player_piece(player.next_player(), Piece::WPawn);

                self[captured_pawn].set_bit(captured_idx, true);
            }
            PieceMoveType::Castling => {
                self[piece].set_bit(from_idx, true);

                // Get rook position depending on if this was kingside or queenside castle
                let (rook_from, rook_to) = if piece_move.to.file > piece_move.from.file {
                    (TilePos::new(7, piece_move.from.rank), TilePos::new(5, piece_move.from.rank))
                } else {
                    (TilePos::new(0, piece_move.from.rank), TilePos::new(3, piece_move.from.rank))
                };

                // Clear the rook from position and set its to position
                let rook = Piece::get_player_piece(player, Piece::WRook);
                self[rook].set_bit(rook_to.to_index(), false);
                self[rook].set_bit(rook_from.to_index(), true);
            }
            PieceMoveType::Promotion(_) => {
                let pawn = Piece::get_player_piece(player, Piece::WPawn);
                self[pawn].set_bit(from_idx, true);
            }
        }

        if let Some(captured) = captured_piece {
            self[captured].set_bit(to_idx, true);
        }

        if let Some(en_passant_tile) = en_passant_tile {
            self.en_passant_tile = 1_u64 << en_passant_tile.to_index();
        }

        self.castling_rights = castling_rights;

        // Only increment the player if the game didn't end on this move
        if self.has_game_ended().is_none() {
            self.next_player();
        }

        Some(history_move)
    }

    // END  ---- Apply and Undo Moves ----------------------------------------------------------------------------------------------------------------

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

    // Pseudolegal Move Generation -------------------------------------------------------------------------------------------------------------------

    /// # Panics
    /// Panics if the player cannot be found from the piece at from
    #[must_use]
    pub fn get_pawn_moves(&self, from: TilePos) -> BitBoard {
        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from");

        get_pawn_moves_for_player(self, from, player, false)
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
            .expect("Could not get player from Piece at from");

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
        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from");

        let mut moves: BitBoard = get_king_moves_no_castling(self, from);

        // Castling TODO Remove duplicated parts of this code
        match player {
            Player::White => {
                // Kingside castle
                if self.castling_rights[0].0
                    && self.is_empty_between(5, 6)
                    && (self.get_attacked_tiles(player) & self.get_between_mask(5, 6)).bits() == 0
                {
                    moves |= 1 << 6; // G1
                }

                // Queenside castle
                if self.castling_rights[0].1
                    && self.is_empty_between(1, 3)
                    && (self.get_attacked_tiles(player) & self.get_between_mask(1, 3)).bits() == 0
                {
                    moves |= 1 << 2; // C1
                }
            }
            Player::Black => {
                // Kingside castle
                if self.castling_rights[1].0
                    && self.is_empty_between(61, 62)
                    && (self.get_attacked_tiles(player) & self.get_between_mask(61, 62)).bits() == 0
                {
                    moves |= 1 << 62; // G1
                }

                // Queenside castle
                if self.castling_rights[1].1
                    && self.is_empty_between(57, 59)
                    && (self.get_attacked_tiles(player) & self.get_between_mask(57, 59)).bits() == 0
                {
                    moves |= 1 << 58; // C1
                }
            }
        }

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

    /// # Panics
    /// Panics if ``Player`` can't be found using ``from``
    #[must_use]
    pub fn get_pawn_pseudolegal_capturing_moves(&self, from: TilePos) -> BitBoard {
        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from");

        get_pawn_moves_for_player(self, from, player, true)
    }

    /// # Panics
    /// Panics if ``Player`` can't be found using ``from``
    #[must_use]
    pub fn get_king_pseudolegal_moves(&self, from: TilePos) -> BitBoard {
        let player = self
            .get_piece(from)
            .to_player()
            .expect("Could not get player from Piece at from");

        get_pawn_moves_for_player(self, from, player, true)
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
            // Check for opposing player's attacks
            if PIECES[i].is_player(player) {
                continue;
            }

            let mut bits = board.bits();
            while bits != 0 {
                let from = bits.trailing_zeros();

                bits &= bits - 1; // Clear LSB

                let from_pos = TilePos::from_index(from);

                // Only count capturing moves for pawns
                attacked |= if PIECES[i] == Piece::get_player_piece(player.next_player(), Piece::WPawn) {
                    self.get_pawn_pseudolegal_capturing_moves(from_pos).bits()
                }
                // Don't count castling moves for king
                else if PIECES[i] == Piece::get_player_piece(player.next_player(), Piece::WKing) {
                    self.get_king_pseudolegal_moves(from_pos).bits()
                } else {
                    self.get_pseudolegal_moves(from_pos).bits()
                };
            }
        }

        attacked.into()
    }

    #[must_use]
    pub fn is_pos_attacked(&self, pos: TilePos) -> bool {
        (self.get_piece(pos).to_player().map_or_else(
            || self.get_attacked_tiles(Player::White) | self.get_attacked_tiles(Player::Black),
            |player| self.get_attacked_tiles(player),
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
                .expect("Could not get player from Piece at from");

            if let Some(king_pos) = self.get_king_pos(player) {
                if !self.move_makes_pos_attacked(new_move, king_pos) {
                    moves.push(new_move);
                }
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
    pub fn get_king_pos(&self, player: Player) -> Option<TilePos> {
        self[self.get_player_king(player)].to_tile_positions().first().copied() // Should always have a king
    }

    // Gets a mask for the files which are between two file indices
    #[must_use]
    pub fn get_between_mask(&self, start: u32, end: u32) -> BitBoard {
        (((1u64 << (end + 1)) - 1) ^ ((1u64 << start) - 1)).into()
    }

    // Checks if indexes are empty between two files/ranks
    #[must_use]
    pub fn is_empty_between(&self, start: u32, end: u32) -> bool {
        let mask = self.get_between_mask(start, end);
        (self.get_occupied() & mask).bits() == 0
    }

    #[must_use]
    pub fn has_game_ended(&self) -> Option<Option<Player>> {
        // Get the position of all kings
        for (player, king_pos) in PLAYERS.iter().map(|&player| (player, self.get_king_pos(player))) {
            // No moves for this player
            if let Some(king_pos) = king_pos {
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
                        return Some(Some(player.next_player()));
                    }

                    // Stalemate
                    return Some(None);
                }
            } else {
                // No king was found for this player
                return Some(Some(player.next_player()));
            }
        }

        None
    }
}

impl fmt::Display for Board {
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

impl std::ops::Index<Piece> for Board {
    type Output = BitBoard;

    fn index(&self, piece: Piece) -> &Self::Output {
        match piece {
            Piece::None => panic!("Tried to use Piece::None as an index"),
            _ => &self.boards[piece.to_bitboard_index()],
        }
    }
}

impl std::ops::IndexMut<Piece> for Board {
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
    only_captures: bool,
) -> BitBoard {
    // Pushing
    let (single_push, double_push) = if only_captures {
        (0, 0)
    } else {
        (
            shift_i8(pawns, forward) & empty,
            shift_i8(
                shift_i8(pawns & Board::get_rank_mask(start_rank).bits(), forward) & empty,
                forward,
            ) & empty,
        )
    };

    // Don't check for opposing pieces when you want all the attacking pieces
    let opposing_pieces = if only_captures { u64::MAX } else { opposing_pieces };

    // Capturing
    let left_capture = shift_i8(pawns, left_shift) & opposing_pieces & !left_file_mask;
    let right_capture = shift_i8(pawns, right_shift) & opposing_pieces & !right_file_mask;

    // En Passant Capturing
    let en_passant_capture_left = shift_i8(pawns, left_shift) & en_passant_tile & !left_file_mask;
    let en_passant_capture_right = shift_i8(pawns, right_shift) & en_passant_tile & !right_file_mask;

    (single_push | double_push | left_capture | right_capture | en_passant_capture_left | en_passant_capture_right).into()
}

fn get_pawn_moves_for_player(bitboards: &Board, from: TilePos, player: Player, only_captures: bool) -> BitBoard {
    let pawn = 1 << from.to_index();

    match player {
        Player::White => get_pawn_moves_for(
            pawn,
            bitboards.get_player_occupied(Player::Black).bits(),
            !bitboards.get_occupied().bits(),
            bitboards.en_passant_tile,
            8,
            1,
            7,
            9,
            Board::get_file_mask(7).bits(),
            Board::get_file_mask(0).bits(),
            only_captures,
        ),
        Player::Black => get_pawn_moves_for(
            pawn,
            bitboards.get_player_occupied(Player::White).bits(),
            !bitboards.get_occupied().bits(),
            bitboards.en_passant_tile,
            -8,
            6,
            -9,
            -7,
            Board::get_file_mask(7).bits(),
            Board::get_file_mask(0).bits(),
            only_captures,
        ),
    }
}

fn get_king_moves_no_castling(bitboards: &Board, from: TilePos) -> BitBoard {
    let file_a: u64 = Board::get_file_mask(0).bits();
    let file_h: u64 = Board::get_file_mask(7).bits();

    let player = bitboards
        .get_piece(from)
        .to_player()
        .expect("Could not get player from Piece at from");
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
    moves & !bitboards.get_player_occupied(player)
}

const fn sliding_moves_in_direction(mut position: u64, occupied: u64, shift_amt: i8, edge_mask: u64) -> u64 {
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
            .find_map(|(i, test_player)| if test_player == self { Some(i) } else { None })
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
        Ok(format!("{}{}", (b'a' + u8::try_from(self.file)?) as char, self.rank + 1))
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
