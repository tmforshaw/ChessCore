use crate::{
    bitboards::BitBoards,
    board::{Board, TilePos},
    piece::Piece,
    piece_move::PieceMove,
};

/// # Panics
/// If ``Piece`` at ``from`` is ``Piece::None``
#[must_use]
pub fn get_pseudolegal_moves(board: &Board, from: TilePos) -> Vec<TilePos> {
    let piece = board.get_piece(from);

    (match piece {
        Piece::BPawn | Piece::WPawn => BitBoards::get_pawn_moves,
        Piece::BKnight | Piece::WKnight => BitBoards::get_knight_moves,
        Piece::BKing | Piece::WKing => BitBoards::get_king_moves,
        Piece::BRook | Piece::WRook => BitBoards::get_rook_moves,
        Piece::BBishop | Piece::WBishop => BitBoards::get_bishop_moves,
        Piece::BQueen | Piece::WQueen => BitBoards::get_queen_moves,
        Piece::None => {
            panic!("Tried to get pseudolegal moves of Piece::None");
        }
    })(&board.positions, from)
    .to_tile_positions()
}

#[must_use]
pub fn get_possible_moves(board: &Board, from: TilePos) -> Option<Vec<TilePos>> {
    let player = board.get_piece(from).to_player()?;

    Some(
        board
            .positions
            .get_possible_moves(from)
            .into_iter()
            .filter(|&piece_move| {
                !board
                    .positions
                    .move_makes_pos_attacked(piece_move, board.get_king_pos(player))
                // && board.get_piece(piece_move.to).to_player() != Some(player)
            })
            .map(|piece_move| piece_move.to)
            .collect::<Vec<_>>(),
    )

    // // Don't allow moves which cause the king to be attacked
    // Some(get_pseudolegal_moves(board, from)
    //     .into_iter()
    //     .filter(|&move_to_pos| {
    //         // Ensure that move won't cause the king to be attacked
    //         !board.positions.move_makes_pos_attacked(PieceMove::new(from, move_to_pos), board.get_king_pos(player))
    //         // Ensure that the piece cant capture its own piece
    //        && board.get_piece(move_to_pos).to_player()
    //             != Some(player)
    //     })
    //     .collect::<Vec<_>>())
}
