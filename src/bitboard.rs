use std::{
    fmt::{self, Display},
    ops,
};

use crate::{
    board::{BOARD_SIZE, Player, TilePos},
    piece::{COLOUR_AMT, PIECE_AMT, PIECES, Piece},
};

#[derive(Copy, Clone, Default, Eq, PartialEq)]
pub struct BitBoard {
    bits: u64,
}

impl BitBoard {
    #[must_use]
    pub const fn bits(&self) -> u64 {
        self.bits
    }

    #[must_use]
    pub const fn from_bits(bits: u64) -> Self {
        Self { bits }
    }

    pub const fn pop_lsb(&mut self) -> Option<u32> {
        if self.bits() == 0 {
            None
        } else {
            // Index of least significant bit
            let lsb = self.bits.trailing_zeros();

            // Clear least significant bit
            self.bits &= self.bits - 1;

            Some(lsb)
        }
    }

    #[must_use]
    pub const fn get_bit(&self, index: u32) -> bool {
        (self.bits >> index) & 1 == 1
    }

    #[must_use]
    pub const fn get_bit_at(&self, tile_pos: TilePos) -> bool {
        self.get_bit(tile_pos.file + tile_pos.rank * BOARD_SIZE)
    }

    pub const fn set_bit(&mut self, index: u32, value: bool) {
        // Clear the bit, then set it
        self.bits &= !(1 << index);
        self.bits |= (value as u64) << index;
    }

    pub const fn set_bit_at(&mut self, tile_pos: TilePos, value: bool) {
        self.set_bit(tile_pos.file + tile_pos.rank * BOARD_SIZE, value);
    }

    // #[must_use]
    // pub fn get_positions(&self) -> Vec<TilePos> {
    //     let mut positions = Vec::new();

    //     let mut bits = self.bits;
    //     for rank in 0..BOARD_SIZE {
    //         for file in 0..BOARD_SIZE {
    //             // Exit loop when consumed bits
    //             if bits == 0 {
    //                 break;
    //             }

    //             if (bits & 1) > 0 {
    //                 positions.push(TilePos::new(file, rank));
    //             }

    //             bits >>= 1;
    //         }
    //     }

    //     positions
    // }

    pub const fn get_rank(&mut self, rank: u32) -> u64 {
        self.bits & (0xFF << (rank * BOARD_SIZE))
    }

    pub const fn get_file(&mut self, file: u32) -> u64 {
        self.bits & (0x0101_0101_0101_0101 << file)
    }

    pub const fn set_rank(&mut self, rank: u32, rank_value: u8) {
        // Clear rank, then set bits
        self.bits &= !(0xFF << (rank * BOARD_SIZE));
        self.bits |= (rank_value as u64) << (rank * BOARD_SIZE);
    }

    pub const fn set_file(&mut self, file: u32, file_value: u8) {
        // Clear file, then set each bit by spacing out the file_value bits by (BOARD_SIZE - 1) many zeros
        self.bits &= !(0x0101_0101_0101_0101 << file);
        self.bits |= ((file_value as u64) & 1) << file
            | ((file_value as u64) & (1 << 1)) << (BOARD_SIZE - 1 + file)
            | ((file_value as u64) & (1 << 2)) << (2 * (BOARD_SIZE - 1) + file)
            | ((file_value as u64) & (1 << 3)) << (3 * (BOARD_SIZE - 1) + file)
            | ((file_value as u64) & (1 << 4)) << (4 * (BOARD_SIZE - 1) + file)
            | ((file_value as u64) & (1 << 5)) << (5 * (BOARD_SIZE - 1) + file)
            | ((file_value as u64) & (1 << 6)) << (6 * (BOARD_SIZE - 1) + file)
            | ((file_value as u64) & (1 << 7)) << (7 * (BOARD_SIZE - 1) + file);
    }

    pub fn to_tile_positions(&self) -> Vec<TilePos> {
        let mut bits = self.bits;

        let mut positions = Vec::new();
        while bits != 0 {
            let tile = bits.trailing_zeros();
            positions.push(TilePos::from_index(tile));

            bits &= bits - 1;
        }

        positions
    }
}

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut message = String::new();

        for rank in 0..BOARD_SIZE {
            for file in 0..BOARD_SIZE {
                message += format!(
                    "{} ",
                    if (self.bits >> (file + rank * BOARD_SIZE)) & 1 == 1 {
                        '#'
                    } else {
                        '-'
                    }
                )
                .as_str();
            }

            if rank < BOARD_SIZE - 1 {
                message.push('\n');
            }
        }

        write!(f, "{message}")
    }
}

impl From<u64> for BitBoard {
    fn from(bits: u64) -> Self {
        Self { bits }
    }
}

impl std::ops::Shl<u32> for BitBoard {
    type Output = Self;

    fn shl(self, rhs: u32) -> Self {
        Self {
            bits: self.bits << rhs,
        }
    }
}

impl std::ops::ShlAssign<u32> for BitBoard {
    fn shl_assign(&mut self, rhs: u32) {
        self.bits <<= rhs
    }
}

impl std::ops::Shr<u32> for BitBoard {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self {
        Self {
            bits: self.bits >> rhs,
        }
    }
}

impl std::ops::ShrAssign<u32> for BitBoard {
    fn shr_assign(&mut self, rhs: u32) {
        self.bits >>= rhs
    }
}

impl std::ops::BitAnd<u64> for BitBoard {
    type Output = Self;

    fn bitand(self, rhs: u64) -> Self {
        Self {
            bits: self.bits & rhs,
        }
    }
}

impl std::ops::BitAndAssign<u64> for BitBoard {
    fn bitand_assign(&mut self, rhs: u64) {
        self.bits &= rhs
    }
}

impl std::ops::BitAnd<Self> for BitBoard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self {
            bits: self.bits & rhs.bits(),
        }
    }
}

impl std::ops::BitAndAssign<Self> for BitBoard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.bits &= rhs.bits()
    }
}

impl std::ops::BitOr<u64> for BitBoard {
    type Output = Self;

    fn bitor(self, rhs: u64) -> Self {
        Self {
            bits: self.bits | rhs,
        }
    }
}

impl std::ops::BitOrAssign<u64> for BitBoard {
    fn bitor_assign(&mut self, rhs: u64) {
        self.bits |= rhs
    }
}

impl std::ops::BitOr<Self> for BitBoard {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self {
            bits: self.bits | rhs.bits(),
        }
    }
}

impl std::ops::BitOrAssign<Self> for BitBoard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.bits |= rhs.bits()
    }
}

impl std::ops::Not for BitBoard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self { bits: !self.bits }
    }
}

#[derive(Default, Clone, Eq, PartialEq)]
pub struct BitBoards {
    pub boards: [BitBoard; PIECE_AMT * COLOUR_AMT],
    pub en_passant_tile: u64,
}

impl BitBoards {
    pub fn get_rank_mask(rank: u32) -> BitBoard {
        (0xFF << (rank * BOARD_SIZE)).into()
    }

    pub fn get_file_mask(file: u32) -> BitBoard {
        (0x0101_0101_0101_0101 << file).into()
    }

    // #[must_use]
    pub fn get_piece(&self, tile_pos: TilePos) -> Piece {
        for &piece in PIECES {
            if self[piece].get_bit_at(tile_pos) {
                return piece;
            }
        }

        Piece::None
    }

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

    pub fn get_occupied(&self) -> BitBoard {
        let mut occupied = 0.into();
        for board in self.boards {
            occupied |= board.bits();
        }

        occupied
    }

    pub fn get_pawn_moves(&self, from: TilePos) -> BitBoard {
        let player = self.get_piece(from).to_player().unwrap(); // TODO Unwrap used

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
            let single_push = (pawns << forward) & empty;
            let double_push =
                ((((pawns & BitBoards::get_rank_mask(start_rank).bits()) << forward) & empty)
                    << forward)
                    & empty;

            // Capturing
            let left_capture = (pawns << left_shift) & opposing_pieces & !right_file_mask;
            let right_capture = (pawns << right_shift) & opposing_pieces & !left_file_mask;

            // En Passant Capturing
            let en_passant_capture_left =
                (pawns << left_shift) & en_passant_tile & !right_file_mask;
            let en_passant_capture_right =
                (pawns << right_shift) & en_passant_tile & !left_file_mask;

            (single_push
                | double_push
                | left_capture
                | right_capture
                | en_passant_capture_left
                | en_passant_capture_right)
                .into()
        }
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
                Self::get_file_mask(0).bits(),
                Self::get_file_mask(7).bits(),
            ),
            Player::Black => get_pawn_moves_for(
                pawn,
                self.get_player_occupied(Player::White).bits(),
                !self.get_occupied().bits(),
                self.en_passant_tile,
                -8,
                7,
                -9,
                -7,
                Self::get_file_mask(7).bits(),
                Self::get_file_mask(0).bits(),
            ),
        }
    }

    pub fn get_knight_moves(&self, from: TilePos) -> BitBoard {
        let file_a: u64 = BitBoards::get_file_mask(0).bits();
        let file_b: u64 = BitBoards::get_file_mask(1).bits();
        let file_g: u64 = BitBoards::get_file_mask(6).bits();
        let file_h: u64 = BitBoards::get_file_mask(7).bits();

        let player = self.get_piece(from).to_player().unwrap(); // TODO Unwrap used

        let knight: BitBoard = (1 << from.to_index()).into();

        let mut moves: BitBoard = 0.into();

        // 2 up, 1 right (<< 17), no wrap if not on file H
        moves |= (knight << 17) & !file_h;

        // 2 up, 1 left (<< 15), no wrap if not on file A
        moves |= (knight << 15) & !file_a;

        // 1 up, 2 right (<< 10), no wrap if not on files G or H
        moves |= (knight << 10) & !(file_g | file_h);

        // 1 up, 2 left (<< 6), no wrap if not on files A or B
        moves |= (knight << 6) & !(file_a | file_b);

        // 2 down, 1 left (>> 17), no wrap if not on file A
        moves |= (knight >> 17) & !file_a;

        // 2 down, 1 right (>> 15), no wrap if not on file H
        moves |= (knight >> 15) & !file_h;

        // 1 down, 2 left (>> 10), no wrap if not on files A or B
        moves |= (knight >> 10) & !(file_a | file_b);

        // 1 down, 2 right (>> 6), no wrap if not on files G or H
        moves |= (knight >> 6) & !(file_g | file_h);

        // Remove squares occupied by own pieces
        moves & !self.get_player_occupied(player)
    }

    pub fn get_king_moves(&self, from: TilePos) -> BitBoard {
        let file_a: u64 = BitBoards::get_file_mask(0).bits();
        let file_h: u64 = BitBoards::get_file_mask(7).bits();

        let player = self.get_piece(from).to_player().unwrap(); // TODO Unwrap used
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

        let file_a: u64 = BitBoards::get_file_mask(0).bits();
        let file_h: u64 = BitBoards::get_file_mask(7).bits();

        let mut moves = 0.into();

        let mut temp: u64 = 1 << from.to_index();
        while temp != 0 {
            let square = temp.trailing_zeros();
            let from_bit = 1u64 << square;

            moves |= sliding_moves_in_direction(from_bit, occupied, 8, 0); // North
            moves |= sliding_moves_in_direction(from_bit, occupied, -8, 0); // South
            moves |= sliding_moves_in_direction(from_bit, occupied, 1, file_h); // East
            moves |= sliding_moves_in_direction(from_bit, occupied, -1, file_a); // West

            temp &= temp - 1; // Pop the LSB
        }

        moves
    }

    pub fn get_rook_moves(&self, from: TilePos) -> BitBoard {
        self.get_orthogonal_moves(from)
    }

    fn get_diagonal_moves(&self, from: TilePos) -> BitBoard {
        let occupied = self.get_occupied().bits();

        let file_a: u64 = BitBoards::get_file_mask(0).bits();
        let file_h: u64 = BitBoards::get_file_mask(7).bits();

        let mut moves = 0.into();

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

        moves
    }

    pub fn get_bishop_moves(&self, from: TilePos) -> BitBoard {
        self.get_diagonal_moves(from)
    }

    pub fn get_queen_moves(&self, from: TilePos) -> BitBoard {
        self.get_orthogonal_moves(from) | self.get_diagonal_moves(from)
    }

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
}

impl Display for BitBoards {
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

impl ops::Index<Piece> for BitBoards {
    type Output = BitBoard;

    fn index(&self, piece: Piece) -> &Self::Output {
        match piece {
            Piece::None => panic!("Tried to use Piece::None as an index"),
            _ => &self.boards[piece.to_bitboard_index()],
        }
    }
}

impl ops::IndexMut<Piece> for BitBoards {
    fn index_mut(&mut self, piece: Piece) -> &mut Self::Output {
        match piece {
            Piece::None => panic!("Tried to use Piece::None as an index"),
            _ => &mut self.boards[piece.to_bitboard_index()],
        }
    }
}

fn sliding_moves_in_direction(mut position: u64, occupied: u64, shift: i8, edge_mask: u64) -> u64 {
    let mut moves = 0;
    loop {
        // Shift position by one step
        if shift > 0 {
            position = (position << shift) & !edge_mask;
        } else {
            position = (position >> (-shift)) & !edge_mask;
        }

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
