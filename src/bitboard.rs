use std::fmt::{self};

use crate::board::{BOARD_SIZE, TilePos};

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

    #[must_use]
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
        Self { bits: self.bits << rhs }
    }
}

impl std::ops::ShlAssign<u32> for BitBoard {
    fn shl_assign(&mut self, rhs: u32) {
        self.bits <<= rhs;
    }
}

impl std::ops::Shr<u32> for BitBoard {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self {
        Self { bits: self.bits >> rhs }
    }
}

impl std::ops::ShrAssign<u32> for BitBoard {
    fn shr_assign(&mut self, rhs: u32) {
        self.bits >>= rhs;
    }
}

impl std::ops::BitAnd<u64> for BitBoard {
    type Output = Self;

    fn bitand(self, rhs: u64) -> Self {
        Self { bits: self.bits & rhs }
    }
}

impl std::ops::BitAndAssign<u64> for BitBoard {
    fn bitand_assign(&mut self, rhs: u64) {
        self.bits &= rhs;
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
        self.bits &= rhs.bits();
    }
}

impl std::ops::BitOr<u64> for BitBoard {
    type Output = Self;

    fn bitor(self, rhs: u64) -> Self {
        Self { bits: self.bits | rhs }
    }
}

impl std::ops::BitOrAssign<u64> for BitBoard {
    fn bitor_assign(&mut self, rhs: u64) {
        self.bits |= rhs;
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
        self.bits |= rhs.bits();
    }
}

impl std::ops::Not for BitBoard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self { bits: !self.bits }
    }
}
