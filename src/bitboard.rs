use std::{
    fmt::{self, Display},
    ops,
};

use crate::{
    board::{BOARD_SIZE, TilePos},
    piece::{COLOUR_AMT, PIECE_AMT, PIECES, Piece},
};

#[derive(Copy, Clone, Default)]
pub struct BitBoard {
    bits: u64,
}

impl BitBoard {
    #[must_use]
    pub const fn get_bit(&self, index: usize) -> bool {
        (self.bits >> index) & 1 == 1
    }

    #[must_use]
    pub const fn get_bit_at(&self, tile_pos: TilePos) -> bool {
        self.get_bit(tile_pos.file + tile_pos.rank * BOARD_SIZE)
    }

    pub const fn set_bit(&mut self, index: usize, value: bool) {
        // Clear the bit, then set it
        self.bits &= !(1 << index);
        self.bits |= (value as u64) << index;
    }

    pub const fn set_bit_at(&mut self, tile_pos: TilePos, value: bool) {
        self.set_bit(tile_pos.file + tile_pos.rank * BOARD_SIZE, value);
    }

    #[must_use]
    pub fn get_positions(&self) -> Vec<TilePos> {
        let mut positions = Vec::new();

        let mut bits = self.bits;
        for rank in 0..BOARD_SIZE {
            for file in 0..BOARD_SIZE {
                // Exit loop when consumed bits
                if bits == 0 {
                    break;
                }

                if (bits & 1) > 0 {
                    positions.push(TilePos::new(file, rank));
                }

                bits >>= 1;
            }
        }

        positions
    }

    pub const fn set_rank(&mut self, rank: usize, rank_value: u8) {
        // Clear rank, then set bits
        self.bits &= !(0xFF << (rank * BOARD_SIZE));
        self.bits |= (rank_value as u64) << (rank * BOARD_SIZE);
    }

    pub const fn set_file(&mut self, file: usize, file_value: u8) {
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

#[derive(Default, Clone)]
pub struct BitBoards {
    pub boards: [BitBoard; PIECE_AMT * COLOUR_AMT],
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
