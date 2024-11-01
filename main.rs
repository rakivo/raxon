macro_rules! static_assert {
    ($constexpr: expr, $($msg: tt) *) => { const _: () = assert!($constexpr, $($msg) *); }
}

static_assert!(Board::WIDTH <= std::mem::size_of::<u64>(), "board width is too big");
static_assert!(Board::HEIGHT <= std::mem::size_of::<u64>(), "board height is too big");

pub type Offset = (i8, i8);

#[repr(packed)]
#[derive(Copy, Clone)]
pub struct Board(u64);

impl Board {
    pub const WIDTH: usize = 8;
    pub const HEIGHT: usize = 8;
    pub const SIZE: usize = Self::WIDTH * Self::HEIGHT;

    #[inline(always)]
    pub const fn new() -> Self {
        Self(0x0)
    }

    #[inline(always)]
    pub const fn pawns() -> Self {
        Self(0x00FF_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn knights() -> Self {
        Self(0x4200_0000_0000_0042)
    }

    #[inline(always)]
    pub const fn rooks() -> Self {
        Self(0x8100_0000_0000_0081)
    }

    #[inline(always)]
    pub const fn bishops() -> Self {
        Self(0x2400_0000_0000_0024)
    }

    #[inline(always)]
    pub const fn queens() -> Self {
        Self(0x0800_0000_0000_0008)
    }

    #[inline(always)]
    pub const fn kings() -> Self {
        Self(0x1000_0000_0000_0010)
    }

    #[inline(always)]
    pub const fn clear(&mut self) {
        self.0 = 0u64
    }

    #[inline(always)]
    pub const fn pos2d_flatten((row, col): (usize, usize)) -> usize {
        row * Self::WIDTH + col
    }

    #[inline(always)]
    pub const fn get_bit2d(&self, row_col: (usize, usize)) -> bool {
        self.get_bit(Self::pos2d_flatten(row_col))
    }

    #[inline(always)]
    pub const fn get_bit(&self, pos: usize) -> bool {
        self.0 & (1 << pos as u8) != 0u64
    }

    #[inline(always)]
    pub const fn set_bit2d(&mut self, row_col: (usize, usize), value: bool) {
        self.set_bit(Self::pos2d_flatten(row_col), value)
    }

    #[inline(always)]
    pub const fn set_bit(&mut self, pos: usize, value: bool) {
        self.0 = (self.0 & !(1 << pos as u8)) | ((value as u64) << pos as u8)
    }

    #[inline(always)]
    pub const fn iter(&self) -> BoardIterator {
        BoardIterator {
            board: *self,
            pos: 0
        }
    }

    pub const fn from_offsets(offsets: &[Offset], pos: usize) -> Self {
        let row = (pos / Self::WIDTH) as i8;
        let col = (pos % Self::HEIGHT) as i8;

        let mut i = 0;
        let mut board = Self::new();
        while i < offsets.len() {
            let (dr, dc) = offsets[i];
            let new_row = row + dr;
            let new_col = col + dc;
            if !(new_row < 0 || new_col < 0) {
                let new_row = new_row as usize;
                let new_col = new_col as usize;
                if new_row < Self::WIDTH && new_col < Self::HEIGHT {
                    board.set_bit2d((new_row, new_col), true);
                }
            } i += 1;
        }

        board
    }

    #[inline(always)]
    pub const fn from_offsets_for_each(offsets: &[Offset]) -> [Self; Self::SIZE] {
        let mut pos = 0;
        let mut boards = [Self::new(); Self::SIZE];
        while pos < 64 {
            boards[pos] = Self::from_offsets(&offsets, pos);
            pos += 1;
        } boards
    }
}

pub struct BoardIterator {
    board: Board,
    pos: usize
}

impl Iterator for BoardIterator {
    type Item = usize;

    fn next(&mut self) -> Option::<Self::Item> {
        while self.pos < Board::SIZE {
            let curr_pos = self.pos;
            self.pos += 1;
            if self.board.get_bit(curr_pos) {
                return Some(curr_pos)
            }
        }
        None
    }
}

#[repr(u8)]
pub enum Pieces {
    WhitePawns   = 0,
    BlackPawns   = 1,
    WhiteRooks   = 2,
    BlackRooks   = 3,
    WhiteKnights = 4,
    BlackKnights = 5,
    WhiteBishops = 6,
    BlackBishops = 7,
    WhiteQueens  = 8,
    BlackQueens  = 9,
    WhiteKings   = 10,
    BlackKings   = 11,

    ReservedPiecesCount = 12
}

impl Pieces {
    pub const fn init_board(&self) -> Board {
        match self {
            Self::WhitePawns   => Board::pawns(),
            Self::BlackPawns   => Board::pawns(),
            Self::WhiteRooks   => Board::rooks(),
            Self::BlackRooks   => Board::rooks(),
            Self::WhiteKnights => Board::knights(),
            Self::BlackKnights => Board::knights(),
            Self::WhiteBishops => Board::bishops(),
            Self::BlackBishops => Board::bishops(),
            Self::WhiteQueens  => Board::queens(),
            Self::BlackQueens  => Board::queens(),
            Self::WhiteKings   => Board::kings(),
            Self::BlackKings   => Board::kings(),
            _ => unreachable!()
        }
    }
}

pub struct Boards([Board; Pieces::ReservedPiecesCount as _]);

impl Boards {
    #[inline(always)]
    pub const fn new() -> Self {
        Self([
            Pieces::WhitePawns.init_board(),
            Pieces::BlackPawns.init_board(),
            Pieces::WhiteRooks.init_board(),
            Pieces::BlackRooks.init_board(),
            Pieces::WhiteKnights.init_board(),
            Pieces::BlackKnights.init_board(),
            Pieces::WhiteBishops.init_board(),
            Pieces::BlackBishops.init_board(),
            Pieces::WhiteQueens.init_board(),
            Pieces::BlackQueens.init_board(),
            Pieces::WhiteKings.init_board(),
            Pieces::BlackKings.init_board(),
        ])
    }

    #[inline(always)]
    pub const fn pieces(&self, pieces: Pieces) -> &Board {
        &self.0[pieces as usize]
    }

    #[inline(always)]
    pub const fn pieces_mut(&mut self, pieces: Pieces) -> &mut Board {
        &mut self.0[pieces as usize]
    }
}

pub const KNIGHT_OFFSET_BOARDS: [Board; Board::SIZE] = {
    Board::from_offsets_for_each(&[
        (2, 1), (2, -1), (-2, 1), (-2, -1),
        (1, 2), (1, -2), (-1, 2), (-1, -2),
    ])
};

#[inline]
pub fn flatten_to_chess_pos(idx: usize) -> String {
    #[cfg(debug_assertions)]
    if idx >= 64 {
        panic!("idx out of bounds: {idx}")
    }

    let col = (idx % Board::HEIGHT) as u8;
    format!{
        "{col_char}{row}",
        col_char = (b'A' + col) as char,
        row = Board::HEIGHT - (idx / Board::WIDTH)
    }
}

fn main() {
    fn print_knight_possible_moves(square: usize) {
        let board = KNIGHT_OFFSET_BOARDS[square];
        let knight_square = flatten_to_chess_pos(square);
        let moves = board.iter().map(flatten_to_chess_pos).collect::<Vec::<_>>();

        println!("knight square: {knight_square}");
        println!("possible knight moves from square {knight_square}: {moves:?}");
    }

    Board::knights().iter().for_each(print_knight_possible_moves);
}

/* TODO:
    Magic bit boards,
    Offsets for king and pawn
*/
