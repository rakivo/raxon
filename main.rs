use std::fmt::{Debug, Display};

mod psqt;
use psqt::*;
mod magic;
use magic::*;

macro_rules! static_assert {
    ($constexpr: expr, $($msg: tt) *) => { const _: () = assert!($constexpr, $($msg) *); }
}

macro_rules! gen_safe_unsafe_getter {
    (pub const fn $name: tt ($($arg: tt: $ty: ty), *) -> $ret: ty {
        $($getter_code: tt) *
    }, if $safe_condition: expr; $($msg: tt) *) => {
        #[track_caller]
        #[inline(always)]
        #[cfg(debug_assertions)]
        pub const fn $name($($arg: $ty), *) -> $ret {
            if $safe_condition {
                $($getter_code) *
            } else {
                panic!($($msg) *)
            }
        }

        #[inline(always)]
        #[cfg(not(debug_assertions))]
        pub const fn $name($($arg: $ty), *) -> $ret {
            $($getter_code) *
        }
    };
}

macro_rules! gen_from_to_index {
    (if $safe_condition: expr; $($msg: tt) *) => {
        gen_safe_unsafe_getter!{
            pub const fn from_index(idx: usize) -> Self {
                unsafe { std::mem::transmute(idx) }
            }, if $safe_condition; $($msg) *
        }

        #[inline(always)]
        pub const fn to_index(self) -> usize {
            self as _
        }
    };

    ($as_ty: ty, if $safe_condition: expr; $($msg: tt) *) => {
        gen_safe_unsafe_getter!{
            pub const fn from_index(idx: usize) -> Self {
                unsafe { std::mem::transmute(idx as $as_ty) }
            }, if $safe_condition; $($msg) *
        }

        #[inline(always)]
        pub const fn to_index(self) -> usize {
            self as _
        }
    };
}

static_assert!(Board::WIDTH <= std::mem::size_of::<u64>(), "board width is too big");
static_assert!(Board::HEIGHT <= std::mem::size_of::<u64>(), "board height is too big");

pub type Offset = (i8, i8);
pub type Result<T> = std::result::Result::<T, ()>;

pub const KNIGHT_OFFSET_BOARDS: [Board; Board::SIZE] = const {
    Board::from_offsets_for_each(&[
        (2, 1), (2, -1), (-2, 1), (-2, -1),
        (1, 2), (1, -2), (-1, 2), (-1, -2),
    ])
};

pub const KING_OFFSET_BOARDS: [Board; Board::SIZE] = const {
    Board::from_offsets_for_each(&[
        (1, 0), (-1, 0), (0, 1), (0, -1),
        (1, 1), (1, -1), (-1, 1), (-1, -1),
    ])
};

pub const BLACK_PAWN_OFFSET_BOARDS: [Board; Board::SIZE] = const {
    Board::from_offsets_for_each(&[(1, -1), (1, 1)])
};

pub const WHITE_PAWN_OFFSET_BOARDS: [Board; Board::SIZE] = const {
    Board::from_offsets_for_each(&[(-1, -1), (-1, 1)])
};

pub type _Score = i16;

#[repr(packed)]
#[derive(Copy, Clone, Debug)]
pub struct Score(pub _Score, pub _Score);

impl Display for Score {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = self.0;
        let b = self.1;
        write!(f, "w: {w}, b: {b}")
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Piece {
    King   = 0,
    Queen  = 1,
    Rook   = 2,
    Bishop = 3,
    Knight = 4,
    Pawn   = 5,

    #[allow(non_camel_case_types)]
    _RESERVED_PIECE_COUNT = 6
}

impl Piece {
    gen_from_to_index!{u8, if idx < Self::_RESERVED_PIECE_COUNT.to_index(); "invalid idx"}

    #[inline(always)]
    pub const fn iter(&self) -> PieceIterator {
        PieceIterator { piece_pos: self.to_index() }
    }

    #[track_caller]
    #[inline(always)]
    pub const fn from_pieces(pieces: Pieces) -> Self {
        use Pieces::*;
        match pieces {
            WhitePawns   => Self::Pawn,
            BlackPawns   => Self::Pawn,
            WhiteRooks   => Self::Rook,
            BlackRooks   => Self::Rook,
            WhiteKnights => Self::Knight,
            BlackKnights => Self::Knight,
            WhiteBishops => Self::Bishop,
            BlackBishops => Self::Bishop,
            WhiteQueens  => Self::Queen,
            BlackQueens  => Self::Queen,
            WhiteKings   => Self::King,
            BlackKings   => Self::King,
            _ => unreachable!()
        }
    }
}

pub struct PieceIterator {
    piece_pos: usize
}

impl Iterator for PieceIterator {
    type Item = Piece;

    fn next(&mut self) -> Option::<Self::Item> {
        if self.piece_pos < Piece::_RESERVED_PIECE_COUNT.to_index() {
            let pos = self.piece_pos;
            self.piece_pos += 1;
            Some(unsafe { std::mem::transmute(pos as u8) })
        } else {
            None
        }
    }
}

#[repr(u8)]
#[derive(Eq, Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Square {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
}

impl Square {
    gen_from_to_index!{u8, if Self::H1.to_index() >= idx; "invalid idx"}

    #[inline(always)]
    pub const fn try_from_2d((row, col): (u8, u8)) -> Result::<Self> {
        if row < Board::HEIGHT as _ && col < Board::WIDTH as _ {
            Ok(unsafe { std::mem::transmute(row * Board::HEIGHT as u8 + col) })
        } else {
            Err(())
        }
    }

    #[inline(always)]
    pub const fn file(&self) -> usize {
        self.to_index() & 7
    }

    #[inline(always)]
    pub const fn rank(&self) -> usize {
        self.to_index() >> 3
    }

    #[inline(always)]
    pub const fn flip_rank(&self) -> usize { // Swap A1 <-> A8
        self.to_index() ^ Self::A8.to_index()
    }

    #[inline(always)]
    pub const fn flip_file(&self) -> usize { // Swap A1 <-> H1
        self.to_index() ^ Self::H1.to_index()
    }
}

impl Display for Square {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let idx = *self as usize;
        let file = (idx % 8) as u8;
        write!{
            f,
            "{file_char}{rank}",
            file_char = (b'a' + file) as char,
            rank = Board::WIDTH - idx / Board::HEIGHT
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
    pub const fn white_pawns() -> Self {
        Self(0x0000_0000_0000_FF00)
    }

    #[inline(always)]
    pub const fn black_pawns() -> Self {
        Self(0x00FF_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn white_knights() -> Self {
        Self(0x0000_0000_0000_0042)
    }

    #[inline(always)]
    pub const fn black_knights() -> Self {
        Self(0x4200_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn white_rooks() -> Self {
        Self(0x0000_0000_0000_0081)
    }

    #[inline(always)]
    pub const fn black_rooks() -> Self {
        Self(0x8100_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn white_bishops() -> Self {
        Self(0x0000_0000_0000_0024)
    }

    #[inline(always)]
    pub const fn black_bishops() -> Self {
        Self(0x2400_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn white_queen() -> Self {
        Self(0x0000_0000_0000_0008)
    }

    #[inline(always)]
    pub const fn black_queen() -> Self {
        Self(0x0800_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn white_king() -> Self {
        Self(0x0000_0000_0000_0010)
    }

    #[inline(always)]
    pub const fn black_king() -> Self {
        Self(0x1000_0000_0000_0000)
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
    pub const fn clear_bit_square(&mut self, pos: Square) {
        self.clear_bit(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn clear_bit(&mut self, pos: usize) {
        self.set_bit(pos, false);
    }

    #[inline(always)]
    pub const fn get_bit_square(&self, pos: Square) -> bool {
        self.get_bit(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn get_bit(&self, pos: usize) -> bool {
        self.0 & (1 << pos as u8) != 0u64
    }

    #[inline(always)]
    pub const fn set_bit_square(&mut self, pos: Square, value: bool) {
        self.set_bit(pos.to_index() as _, value)
    }

    #[inline(always)]
    pub const fn set_bit(&mut self, pos: usize, value: bool) {
        self.0 = (self.0 & !(1 << pos as u8)) | ((value as u64) << pos as u8)
    }

    #[inline(always)]
    pub const fn get_white_pawn_attacks(square: usize) -> Self {
        WHITE_PAWN_OFFSET_BOARDS[square]
    }

    #[inline(always)]
    pub const fn get_black_pawn_attacks(square: usize) -> Self {
        BLACK_PAWN_OFFSET_BOARDS[square]
    }

    #[inline(always)]
    pub const fn get_king_attacks(square: usize) -> Self {
        KING_OFFSET_BOARDS[square]
    }

    #[inline(always)]
    pub const fn get_queen_attacks(&self, square: usize) -> Self {
        Self(self.get_rook_attacks(square).0 | self.get_bishop_attacks(square).0)
    }

    #[inline(always)]
    pub const fn get_knight_attacks(square: usize) -> Self {
        KNIGHT_OFFSET_BOARDS[square]
    }

    #[inline(always)]
    pub const fn get_rook_attacks(&self, square: usize) -> Self {
        let Board(mut occupancy) = *self;

	    occupancy &= ROOK_MASKS[square];
	    occupancy *= ROOK_MAGICS[square];
	    occupancy >>= Board::SIZE as u8 - ROOK_RELEVANT_BITS[square];

	    Self(ROOK_ATTACKS[square][occupancy as usize])
    }

    #[inline(always)]
    pub const fn get_bishop_attacks(&self, square: usize) -> Self {
        let Board(mut occupancy) = *self;

	    occupancy &= BISHOP_MASKS[square];
	    occupancy *= BISHOP_MAGICS[square];
	    occupancy >>= Board::SIZE as u8 - BISHOP_RELEVANT_BITS[square];

	    Self(BISHOP_ATTACKS[square][occupancy as usize])
    }

    #[inline(always)]
    pub const fn get_white_pawn_attacks_square(&self, pos: Square) -> Self {
        Self::get_white_pawn_attacks(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn get_black_pawn_attacks_square(&self, pos: Square) -> Self {
        Self::get_black_pawn_attacks(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn get_king_attacks_square(&self, pos: Square) -> Self {
        Self::get_king_attacks(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn get_queen_attacks_square(&self, pos: Square) -> Self {
        self.get_queen_attacks(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn get_knight_attacks_square(pos: Square) -> Self {
        Self::get_knight_attacks(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn get_bishop_attacks_square(&self, pos: Square) -> Self {
        self.get_bishop_attacks(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn get_rook_attacks_square(&self, pos: Square) -> Self {
        self.get_rook_attacks(pos.to_index() as _)
    }

    #[inline(always)]
    pub const fn count(&self) -> usize {
        self.0.count_ones() as _
    }

    #[inline(always)]
    pub const fn is_empty(&self) -> bool {
        self.count() == 0
    }

    #[inline(always)]
    pub const fn iter(&self) -> BoardIterator {
        BoardIterator {
            board: *self,
            pos: 0
        }
    }

    pub const fn from_offsets(offsets: &[Offset], pos: usize) -> Self {
        let row = (pos / Self::HEIGHT) as i8;
        let col = (pos % Self::WIDTH) as i8;

        let mut i = 0;
        let mut board = Self::new();
        while i < offsets.len() {
            let (dr, dc) = offsets[i];
            let new_row = row + dr;
            let new_col = col + dc;
            if !(new_row < 0 || new_col < 0) {
                if let Ok(mov) = Square::try_from_2d((new_row as u8, new_col as u8)) {
                    board.set_bit_square(mov, true);
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
        write!(f, "     bitboard: 0x{bitboard:X}")
    }
}

impl Debug for Board {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
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
#[derive(Copy, Clone, Debug)]
pub enum Pieces {
    WhitePawns   = 0,
    BlackPawns   = 1,
    WhiteKnights = 2,
    BlackKnights = 3,
    WhiteBishops = 4,
    BlackBishops = 5,
    WhiteRooks   = 6,
    BlackRooks   = 7,
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
    gen_from_to_index!{u8, if Self::_RESERVED_PIECES_COUNT.to_index() > idx; "invalid idx"}

    #[inline(always)]
    pub const fn piece(&self) -> Piece {
        Piece::from_pieces(*self)
    }

    #[inline(always)]
    pub const fn is_white(&self) -> bool {
        matches!(self,
                 Self::WhitePawns   | Self::WhiteRooks   |
                 Self::WhiteKnights | Self::WhiteBishops |
                 Self::WhiteQueens  | Self::WhiteKings)
    }

    #[inline(always)]
    pub const fn is_black(&self) -> bool {
        !self.is_white()
    }

    #[track_caller]
    #[inline(always)]
    pub const fn to_char(&self) -> char {
        use Pieces::*;
        match self {
            WhitePawns   => 'P',
            BlackPawns   => 'p',
            WhiteRooks   => 'R',
            BlackRooks   => 'r',
            WhiteKnights => 'N',
            BlackKnights => 'n',
            WhiteBishops => 'B',
            BlackBishops => 'b',
            WhiteQueens  => 'Q',
            BlackQueens  => 'q',
            WhiteKings   => 'K',
            BlackKings   => 'k',
            _ => unreachable!()
        }
    }

    #[track_caller]
    #[inline(always)]
    pub const fn init_board(&self) -> Board {
        match self {
            Self::WhitePawns   => Board::white_pawns(),
            Self::BlackPawns   => Board::black_pawns(),
            Self::WhiteRooks   => Board::white_rooks(),
            Self::BlackRooks   => Board::black_rooks(),
            Self::WhiteKnights => Board::white_knights(),
            Self::BlackKnights => Board::black_knights(),
            Self::WhiteBishops => Board::white_bishops(),
            Self::BlackBishops => Board::black_bishops(),
            Self::WhiteQueens  => Board::white_queen(),
            Self::BlackQueens  => Board::black_queen(),
            Self::WhiteKings   => Board::white_king(),
            Self::BlackKings   => Board::black_king(),
            Self::AllWhites    => Board::all_whites(),
            Self::AllBlacks    => Board::all_blacks(),
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct Boards([Board; Pieces::_RESERVED_PIECES_COUNT.to_index()]);

impl Boards {
    #[inline(always)]
    pub const fn new() -> Self {
        Self([
            Pieces::WhitePawns.init_board(),
            Pieces::BlackPawns.init_board(),
            Pieces::WhiteKnights.init_board(),
            Pieces::BlackKnights.init_board(),
            Pieces::WhiteBishops.init_board(),
            Pieces::BlackBishops.init_board(),
            Pieces::WhiteRooks.init_board(),
            Pieces::BlackRooks.init_board(),
            Pieces::WhiteQueens.init_board(),
            Pieces::BlackQueens.init_board(),
            Pieces::WhiteKings.init_board(),
            Pieces::BlackKings.init_board(),
            Pieces::AllWhites.init_board(),
            Pieces::AllBlacks.init_board(),
        ])
    }

    #[inline]
    pub const fn not_alls_count() -> usize {
        Pieces::_RESERVED_PIECES_COUNT.to_index() - 2
    }

    #[inline(always)]
    pub const fn pieces(&self, pieces: Pieces) -> &Board {
        &self.0[pieces as usize]
    }

    #[inline(always)]
    pub const fn pieces_mut(&mut self, pieces: Pieces) -> &mut Board {
        &mut self.0[pieces as usize]
    }

    #[inline(always)]
    pub const fn pieces_count(&self, pieces: Pieces) -> usize {
        self.0[pieces as usize].count()
    }

    // get piece kind at `src`th bit
    #[track_caller]
    #[inline(always)]
    pub const fn get_kind(&self, src: usize) -> Pieces {
        let mut i = 0;
        while i < Pieces::_RESERVED_PIECES_COUNT.to_index() - 2 {
            if self.0[i].get_bit(src) {
                return Pieces::from_index(i)
            } i += 1;
        } unreachable!()
    }

    #[inline(always)]
    pub fn iter_whites(&self) -> impl Iterator::<Item = Board> + '_ {
        [
            Pieces::WhitePawns,
            Pieces::WhiteKnights,
            Pieces::WhiteBishops,
            Pieces::WhiteRooks,
            Pieces::WhiteQueens,
            Pieces::WhiteKings,
        ].into_iter().map(|pieces| *self.pieces(pieces)).into_iter()
    }

    #[inline(always)]
    pub fn iter_blacks(&self) -> impl Iterator::<Item = Board> + '_ {
        [
            Pieces::BlackPawns,
            Pieces::BlackKnights,
            Pieces::BlackBishops,
            Pieces::BlackRooks,
            Pieces::BlackQueens,
            Pieces::BlackKings,
        ].into_iter().map(|pieces| *self.pieces(pieces)).into_iter()
    }

    #[inline(always)]
    pub fn iter(&self) -> impl Iterator::<Item = Board> + '_ {
        self.iter_whites().chain(self.iter_blacks()).into_iter()
    }

    #[inline(always)]
    pub const fn phase(&self) -> _Score {
        const KNIGHT_WEIGHT: _Score = 1;
        const BISHOP_WEIGHT: _Score = 1;
        const ROOK_WEIGHT:   _Score = 2;
        const QUEEN_WEIGHT:  _Score = 4;

        const MAX_PHASE: _Score = 2 * (KNIGHT_WEIGHT +
                                       BISHOP_WEIGHT +
                                       ROOK_WEIGHT)  + QUEEN_WEIGHT;

        let ret = self.pieces_count(Pieces::WhiteKnights) as _Score * KNIGHT_WEIGHT +
                  self.pieces_count(Pieces::BlackKnights) as _Score * KNIGHT_WEIGHT +
                  self.pieces_count(Pieces::WhiteBishops) as _Score * BISHOP_WEIGHT +
                  self.pieces_count(Pieces::BlackBishops) as _Score * BISHOP_WEIGHT +
                  self.pieces_count(Pieces::WhiteRooks)   as _Score * ROOK_WEIGHT   +
                  self.pieces_count(Pieces::BlackRooks)   as _Score * ROOK_WEIGHT   +
                  self.pieces_count(Pieces::WhiteQueens)  as _Score * QUEEN_WEIGHT  +
                  self.pieces_count(Pieces::BlackQueens)  as _Score * QUEEN_WEIGHT;

        if ret < MAX_PHASE { ret } else { MAX_PHASE }
    }

    #[inline(always)]
    pub const fn make_move_index(&mut self, src: usize, dst: usize) {
        let kind = self.get_kind(src);
        let board = self.pieces_mut(kind);

        board.clear_bit(src);
        board.set_bit(dst, true);

        if kind.is_white() {
            self.pieces_mut(Pieces::AllWhites).clear_bit(src);
            self.pieces_mut(Pieces::AllWhites).set_bit(dst, true);
        } else {
            self.pieces_mut(Pieces::AllBlacks).clear_bit(src);
            self.pieces_mut(Pieces::AllBlacks).set_bit(dst, true);
        }
    }

    #[inline(always)]
    pub const fn make_move(&mut self, src: Square, dst: Square) {
        self.make_move_index(src.to_index(), dst.to_index())
    }

    #[inline]
    pub fn evaluate(&self) -> Score {
        const fn next(board: &mut Board) -> usize {
            let pos = board.0.trailing_zeros();
            board.0 ^= 0x1u64 << pos;
            pos as _
        }

        let (mut wscore, mut bscore) = (0, 0);
        self.iter_whites().zip(self.iter_blacks()).enumerate().for_each(|(piece_idx, (mut w, mut b))| {
            wscore += std::iter::from_fn(move || {
                if w.0 != 0 {
                    Some(PSQT_MG[piece_idx][FLIP[next(&mut w)]])
                } else {
                    None
                }
            }).sum::<_Score>();

            bscore += std::iter::from_fn(move || {
                if b.0 != 0 {
                    Some(PSQT_MG[piece_idx][next(&mut b)])
                } else {
                    None
                }
            }).sum::<_Score>();
        });

        Score(wscore, bscore)
    }
}

impl std::fmt::Display for Boards {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut char_board = ['.'; 64];
        for (idx, board) in self.0.iter().take(Pieces::_RESERVED_PIECES_COUNT.to_index() - 2).enumerate() {
            let piece_char = Pieces::from_index(idx).to_char();

            for pos in board.iter() {
                char_board[pos] = piece_char;
            }
        }

        writeln!(f, " +-----------------+")?;
        for rank in (0..8).rev() {
            write!(f, " |")?;
            for file in 0..8 {
                let pos = rank * Board::WIDTH + file;
                write!(f, " {0}", char_board[pos])?;
            }
            writeln!(f, " | {0}", rank + 1)?;
        }
        writeln!(f, " +-----------------+")?;
        write!(f, "   a b c d e f g h")
    }
}

fn main() {
    let mut boards = Boards::new();
    // boards.make_move(Square::E2, Square::E4);
    // boards.make_move(Square::D7, Square::D5);
    // boards.make_move(Square::B1, Square::C3);
    // boards.make_move(Square::G8, Square::F6);
    // boards.make_move(Square::F2, Square::F3);
    // boards.make_move(Square::F2, Square::F3);

    boards.make_move(Square::D2, Square::D4);
    boards.make_move(Square::E2, Square::E4);
    boards.make_move(Square::F2, Square::F4);

    // boards.make_move(Square::D8, Square::D5);
    // println!("A8: {}", Square::A8.to_index());

    println!("{boards}");
    println!("{}", boards.evaluate());
}
