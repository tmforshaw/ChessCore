#[cfg(test)]
#[test]
fn empty_between_test() {
    use crate::{
        board::{Board, CASTLING_FEN, TilePos},
        piece_move::PieceMove,
    };

    let mut board = Board::from_fen(CASTLING_FEN).expect("Could not create board from FEN");

    board.apply_move(PieceMove::new(TilePos::new(4, 0), TilePos::new(6, 0)));

    let castling_rights = board.positions.castling_rights;
    assert!(castling_rights[0] == (false, false));

    // Test white king castling rights

    let mut board = Board::from_fen(CASTLING_FEN).expect("Could not create board from FEN");
    board.apply_move(PieceMove::new(TilePos::new(0, 0), TilePos::new(1, 0)));

    let castling_rights = board.positions.castling_rights;
    assert!(castling_rights[0] == (true, false));

    let history_move = board.move_history.traverse_prev().expect("Move history empty");
    board.undo_move(history_move);

    let castling_rights = board.positions.castling_rights;
    assert!(castling_rights[0] == (true, true));

    let mut board = Board::from_fen(CASTLING_FEN).expect("Could not create board from FEN");
    board.apply_move(PieceMove::new(TilePos::new(4, 0), TilePos::new(5, 0)));

    let castling_rights = board.positions.castling_rights;
    assert!(castling_rights[0] == (false, false));

    // Test black king castling rights

    let mut board = Board::from_fen(CASTLING_FEN).expect("Could not create board from FEN");
    board.apply_move(PieceMove::new(TilePos::new(0, 7), TilePos::new(1, 7)));

    let castling_rights = board.positions.castling_rights;
    assert!(castling_rights[1] == (true, false));

    let history_move = board.move_history.traverse_prev().expect("Move history empty");
    board.undo_move(history_move);

    let castling_rights = board.positions.castling_rights;
    assert!(castling_rights[1] == (true, true));

    let mut board = Board::from_fen(CASTLING_FEN).expect("Could not create board from FEN");
    board.apply_move(PieceMove::new(TilePos::new(4, 7), TilePos::new(5, 7)));

    let castling_rights = board.positions.castling_rights;
    assert!(castling_rights[1] == (false, false));
}
