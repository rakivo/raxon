use std::fmt::Display;

mod magic;
use magic::*;

macro_rules! static_assert {
    ($constexpr: expr, $($msg: tt) *) => { const _: () = assert!($constexpr, $($msg) *); }
}

static_assert!(Board::WIDTH <= std::mem::size_of::<u64>(), "board width is too big");
static_assert!(Board::HEIGHT <= std::mem::size_of::<u64>(), "board height is too big");

pub type Offset = (i8, i8);

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum Move {
    A8, B8, C8, D8, E8, F8, G8, H8,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A1, B1, C1, D1, E1, F1, G1, H1,
}

impl Move {
    pub const fn flatten(&self) -> u8 {
        *self as _
    }

    #[inline(always)]
    pub const fn try_from_flatten(idx: u8) -> Result::<Self, ()> {
        if Self::H1 as u8 >= idx {
            Ok(unsafe { std::mem::transmute(idx) })
        } else {
            Err(())
        }
    }

    #[inline(always)]
    pub const fn try_from_2d((row, col): (u8, u8)) -> Result::<Self, ()> {
        if row < 8 && col < 8 {
            Ok(unsafe { std::mem::transmute(row * 8 + col) })
        } else {
            Err(())
        }
    }
}

impl Display for Move {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let idx = *self as u8;
        let file = (idx % 8) as u8;
        write!{
            f,
            "{file_char}{rank}",
            file_char = (b'a' + file) as char,
            rank = (idx / 8) as u8 + 1
        }
    }
}

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
    pub const fn all_whites() -> Self {
        Self(0x0000_0000_0000_FFFF)
    }

    #[inline(always)]
    pub const fn all_blacks() -> Self {
        Self(0xFFFF_0000_0000_0000)
    }
    
    #[inline(always)]
    pub const fn clear(&mut self) {
        *self = Self::new();
    }

    #[inline(always)]
    pub const fn get_bit_move(&self, pos: Move) -> bool {
        self.get_bit(pos.flatten() as _)
    }

    #[inline(always)]
    pub const fn get_bit(&self, pos: usize) -> bool {
        self.0 & (1 << pos as u8) != 0u64
    }

    #[inline(always)]
    pub const fn set_bit_move(&mut self, pos: Move, value: bool) {
        self.set_bit(pos.flatten() as _, value)
    }

    #[inline(always)]
    pub const fn set_bit(&mut self, pos: usize, value: bool) {
        self.0 = (self.0 & !(1 << pos as u8)) | ((value as u64) << pos as u8)
    }

    #[inline]
    pub const fn get_knight_attacks(square: usize) -> Self {
        KNIGHT_OFFSET_BOARDS[square]
    }

    #[inline]
    pub const fn get_rook_attacks(&self, square: usize) -> Self {
        let Board(mut occupancy) = *self;

	    occupancy &= ROOK_MASKS[square];
	    occupancy *=  ROOK_MAGICS[square];
	    occupancy >>= Board::SIZE as u8 - ROOK_RELEVANT_BITS[square];

	    Board(ROOK_ATTACKS[square][occupancy as usize])
    }

    #[inline]
    pub const fn get_bishop_attacks(&self, square: usize) -> Self {
        let Board(mut occupancy) = *self;

	    occupancy &= BISHOP_MASKS[square];
	    occupancy *= BISHOP_MAGICS[square];
	    occupancy >>= Board::SIZE as u8 - BISHOP_RELEVANT_BITS[square];

	    Board(BISHOP_ATTACKS[square][occupancy as usize])
    }

    #[inline]
    pub const fn get_knight_attacks_move(pos: Move) -> Self {
        Self::get_knight_attacks(pos.flatten() as _)
    }

    #[inline]
    pub const fn get_bishop_attacks_move(&self, pos: Move) -> Self {
        self.get_bishop_attacks(pos.flatten() as _)
    }

    #[inline]
    pub const fn get_rook_attacks_move(&self, pos: Move) -> Self {
        self.get_rook_attacks(pos.flatten() as _)
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
                if let Ok(mov) = Move::try_from_2d((new_row as u8, new_col as u8)) {
                    board.set_bit_move(mov, true);
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

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        
        let Board(bitboard) = *self;
        for rank in 0..8 {
            for file in 0..8 {
                let square = rank * 8 + file;
                if file == 0 {
                    write!(f, "  {rank} ", rank = 8 - rank)?;
                }
                write!(f, " {bit}", bit = if (bitboard >> square) & 1 == 1 { 1 } else { 0 })?;
            }
            
            println!();
        }
        
        writeln!(f, "\n     a b c d e f g h")?;
        write!(f, "     bitboard: {bitboard}")
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
        } None
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
    AllWhites    = 12,
    AllBlacks    = 13,

    #[allow(non_camel_case_types)]
    _RESERVED_PIECES_COUNT = 14
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
            Self::AllWhites    => Board::all_whites(),
            Self::AllBlacks    => Board::all_blacks(),
            _ => unreachable!()
        }
    }
}

pub struct Boards([Board; Pieces::_RESERVED_PIECES_COUNT as _]);

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
            Pieces::AllWhites.init_board(),
            Pieces::AllBlacks.init_board(),
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

fn main() {
    let mut board = Board::new();
    board.set_bit_move(Move::E4, true);
    board.set_bit_move(Move::G2, true);
    board.set_bit_move(Move::D3, true);
    board.set_bit_move(Move::B8, true);
    board.set_bit_move(Move::C5, true);

    println!("board occupation: ");
    println!("{board}");

    println!("\nrook attacks: ");
    println!("{}", board.get_rook_attacks_move(Move::E5));

    println!("\nbishop attacks: ");
    println!("{}", board.get_bishop_attacks_move(Move::G6));

    println!("\nknight attacks: ");
    println!("{}", Board::get_knight_attacks_move(Move::G6));
}

/* TODO:
    Magic bit boards,
    Offsets for king and pawn
*/
