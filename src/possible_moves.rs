use crate::{
    board::{Board, TilePos},
    piece::Piece,
    piece_move::PieceMove,
};

#[must_use]
pub fn get_pseudolegal_moves(board: &Board, from: TilePos) -> Option<Vec<TilePos>> {
    let piece = board.get_piece(from);

    (match piece {
        Piece::BQueen | Piece::WQueen => Board::get_ortho_diagonal_moves,
        Piece::BKing | Piece::WKing => Board::get_king_moves,
        Piece::BRook | Piece::WRook => Board::get_orthogonal_moves,
        Piece::BKnight | Piece::WKnight => Board::get_knight_moves,
        Piece::BBishop | Piece::WBishop => Board::get_diagonal_moves,
        Piece::BPawn | Piece::WPawn => Board::get_pawn_moves,
        Piece::None => {
            const fn no_moves(_: &Board, _: TilePos) -> Option<Vec<TilePos>> {
                None
            }

            no_moves
        }
    })(board, from)
}

#[must_use]
pub fn get_possible_moves(board: &Board, from: TilePos) -> Option<Vec<TilePos>> {
    let player = board.get_piece(from).to_player()?;

    // Don't allow moves which cause the king to be attacked
    Some(get_pseudolegal_moves(board, from)?
        .into_iter()
        .filter(|&move_to_pos| {
            // Ensure that move won't cause the king to be attacked
            !board.move_makes_pos_attacked(PieceMove::new(from, move_to_pos), board.get_king_pos(player))
            // Ensure that the piece cant capture its own piece
           && board.get_piece(move_to_pos).to_player()
                != Some(player)
        })
        .collect::<Vec<_>>())
}
