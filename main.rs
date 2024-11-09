use std::fmt::{Debug, Display};

mod psqt;
mod magic;
#[macro_use]
mod macros;

use psqt::*;
use magic::*;

use Square::*;

static_assert!(BitBoard::WIDTH <= std::mem::size_of::<u64>(), "board width is too big");
static_assert!(BitBoard::HEIGHT <= std::mem::size_of::<u64>(), "board height is too big");

pub type Offset = (i8, i8);
pub type Result<T> = std::result::Result::<T, ()>;

pub const KNIGHT_OFFSET_BOARDS: [BitBoard; BitBoard::SIZE] = const {
    BitBoard::from_offsets_for_each(&[
        (2, 1), (2, -1), (-2, 1), (-2, -1),
        (1, 2), (1, -2), (-1, 2), (-1, -2),
    ])
};

pub const KING_OFFSET_BOARDS: [BitBoard; BitBoard::SIZE] = const {
    BitBoard::from_offsets_for_each(&[
        (1, 0), (-1, 0), (0, 1), (0, -1),
        (1, 1), (1, -1), (-1, 1), (-1, -1),
    ])
};

pub type _Score = i16;

#[repr(packed)]
#[derive(Copy, Clone, Debug)]
pub struct Score(pub _Score, pub _Score);

impl Display for Score {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = self.0;
        let b = self.1;
        write!(f, "white: {w}, black: {b}")
    }
}

#[repr(u8)]
#[derive(Eq, Copy, Clone, Debug, PartialEq)]
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
    A8, B8, C8, D8, E8, F8, G8, H8
}

impl Square {
    gen_from_to_index!{u8, if H1.to_index() >= idx; "invalid idx"}

    #[inline(always)]
    pub const fn to_bit_index(&self) -> usize {
        FLIP[self.to_index()]
    }

    #[inline(always)]
    pub const fn file(&self) -> usize {
        self.to_index() & 7
    }

    #[inline(always)]
    pub const fn rank(&self) -> usize {
        self.to_index() >> 3
    }

    // Swap A1 <-> A8
    #[inline(always)]
    pub const fn flip_rank(&self) -> usize {
        self.to_index() ^ A8.to_index()
    }

    // Swap A1 <-> H1
    #[inline(always)]
    pub const fn flip_file(&self) -> usize {
        self.to_index() ^ H1.to_index()
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
            rank = BitBoard::WIDTH - idx / BitBoard::HEIGHT
        }
    }
}

#[repr(packed)]
#[derive(Copy, Clone)]
pub struct BitBoard(u64);

impl BitBoard {
    pub const WIDTH: usize = 8;
    pub const HEIGHT: usize = 8;
    pub const SIZE: usize = Self::WIDTH * Self::HEIGHT;

    #[inline(always)]
    pub const fn new() -> Self {
        Self(0x0)
    }

    #[inline(always)]
    pub const fn white_pawns() -> Self {
        Self(0x00FF_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn black_pawns() -> Self {
        Self(0x0000_0000_0000_FF00)
    }

    #[inline(always)]
    pub const fn white_knights() -> Self {
        Self(0x4200_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn black_knights() -> Self {
        Self(0x0000_0000_0000_0042)
    }

    #[inline(always)]
    pub const fn white_rooks() -> Self {
        Self(0x8100_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn black_rooks() -> Self {
        Self(0x0000_0000_0000_0081)
    }

    #[inline(always)]
    pub const fn white_bishops() -> Self {
        Self(0x2400_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn black_bishops() -> Self {
        Self(0x0000_0000_0000_0024)
    }

    #[inline(always)]
    pub const fn white_queen() -> Self {
        Self(0x0800_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn black_queen() -> Self {
        Self(0x0000_0000_0000_0008)
    }

    #[inline(always)]
    pub const fn white_king() -> Self {
        Self(0x1000_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn black_king() -> Self {
        Self(0x0000_0000_0000_0010)
    }

    #[inline(always)]
    pub const fn all_whites() -> Self {
        Self(0xFFFF_0000_0000_0000)
    }

    #[inline(always)]
    pub const fn all_blacks() -> Self {
        Self(0x0000_0000_0000_FFFF)
    }
    
    #[inline(always)]
    pub const fn clear(&mut self) {
        *self = Self::new()
    }

    #[inline(always)]
    pub const fn clear_bit_square(&mut self, pos: Square) -> &mut Self {
        self.clear_bit(pos.to_bit_index() as _)
    }

    #[inline(always)]
    pub const fn clear_bit(&mut self, pos: usize) -> &mut Self {
        self.set_bit(pos, false)
    }

    #[inline(always)]
    pub const fn get_bit_square(&self, pos: Square) -> bool {
        self.get_bit(pos.to_bit_index() as _)
    }

    #[inline(always)]
    pub const fn get_bit(&self, pos: usize) -> bool {
        (self.0 >> pos) & 1 == 1
    }

    #[inline(always)]
    pub const fn set_bit_square(&mut self, pos: Square, value: bool) -> &mut Self {
        self.set_bit(pos.to_bit_index() as _, value)
    }

    #[inline(always)]
    pub const fn set_bit(&mut self, pos: usize, value: bool) -> &mut Self {
        self.0 = (self.0 & !(1 << pos as u8)) | ((value as u64) << pos as u8);
        self
    }

    #[inline(always)]
    pub const fn get_white_pawn_moves(square: usize, occupancy: Self, friendly: Self) -> Self {
        Self((WHITE_PAWN_MOVES[square] & !friendly.0) | (WHITE_PAWN_ATTACKS[square] & occupancy.0))
    }

    #[inline(always)]
    pub const fn get_black_pawn_moves(square: usize, occupancy: Self, friendly: Self) -> Self {
        Self((BLACK_PAWN_MOVES[square] & !friendly.0) | (BLACK_PAWN_ATTACKS[square] & occupancy.0))
    }

    #[inline(always)]
    pub const fn get_king_moves(square: usize, friendly: Self) -> Self {
        Self(KING_OFFSET_BOARDS[square].0 & !friendly.0)
    }

    #[inline(always)]
    pub const fn get_queen_moves(square: usize, occupancy: Self, friendly: Self) -> Self {
        Self(Self::get_rook_moves(square, occupancy, friendly).0 | Self::get_bishop_moves(square, occupancy, friendly).0)
    }

    #[inline(always)]
    pub const fn get_knight_moves(square: usize, friendly: Self) -> Self {
        Self(KNIGHT_OFFSET_BOARDS[square].0 & !friendly.0)
    }

    #[inline(always)]
    pub const fn get_rook_moves(square: usize, occupancy: Self, friendly: Self) -> Self {
        let Self(mut occupancy) = occupancy;

	    occupancy &= ROOK_MASKS[square];
	    occupancy *= ROOK_MAGICS[square];
	    occupancy >>= BitBoard::SIZE as u8 - ROOK_RELEVANT_BITS[square];

	    Self(ROOK_ATTACKS[square][occupancy as usize] & !friendly.0)
    }

    #[inline(always)]
    pub const fn get_bishop_moves(square: usize, occupancy: Self, friendly: Self) -> Self {
        let Self(mut occupancy) = occupancy;

	    occupancy &= BISHOP_MASKS[square];
	    occupancy *= BISHOP_MAGICS[square];
	    occupancy >>= BitBoard::SIZE as u8 - BISHOP_RELEVANT_BITS[square];

        Self(BISHOP_ATTACKS[square][occupancy as usize] & !friendly.0)
    }

    #[inline(always)]
    pub const fn get_white_pawn_moves_square(pos: Square, occupancy: Self, friendly: Self) -> Self {
        Self::get_white_pawn_moves(pos.to_index() as _, occupancy, friendly)
    }

    #[inline(always)]
    pub const fn get_black_pawn_moves_square(pos: Square, occupancy: Self, friendly: Self) -> Self {
        Self::get_black_pawn_moves(pos.to_index() as _, occupancy, friendly)
    }

    #[inline(always)]
    pub const fn get_king_moves_square(&self, pos: Square, friendly: Self) -> Self {
        Self::get_king_moves(pos.to_bit_index() as _, friendly)
    }

    #[inline(always)]
    pub const fn get_queen_moves_square(pos: Square, occupancy: Self, friendly: Self) -> Self {
        Self::get_queen_moves(pos.to_bit_index() as _, occupancy, friendly)
    }

    #[inline(always)]
    pub const fn get_knight_moves_square(pos: Square, friendly: Self) -> Self {
        Self::get_knight_moves(pos.to_bit_index() as _, friendly)
    }

    #[inline(always)]
    pub const fn get_bishop_moves_square(&self, pos: Square, occupancy: Self, friendly: Self) -> Self {
        Self::get_bishop_moves(pos.to_bit_index() as _, occupancy, friendly)
    }

    #[inline(always)]
    pub const fn get_rook_moves_square(&self, pos: Square, occupancy: Self, friendly: Self) -> Self {
        Self::get_rook_moves(pos.to_bit_index() as _, occupancy, friendly)
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
        let mut i = 0;
        let mut board = Self::new();
        let row = (pos / Self::WIDTH) as i8;
        let col = (pos % Self::WIDTH) as i8;
        while i < offsets.len() {
            let (dr, dc) = offsets[i];
            let new_row = row + dr;
            let new_col = col + dc;
            if new_row >= 0 && new_row < Self::WIDTH as i8 && new_col >= 0 && new_col < Self::WIDTH as i8 {
                let new_pos = (new_row as usize * Self::WIDTH) + new_col as usize;
                board.set_bit(new_pos, true);
            } i += 1;
        } board
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

impl Display for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        
        let BitBoard(bitboard) = *self;
        for rank in 0..8 {
            for file in 0..8 {
                let square = rank * BitBoard::HEIGHT + file;
                if file == 0 {
                    write!(f, "  {rank} ", rank = BitBoard::HEIGHT - rank)?;
                } 
                write!(f, " {bit}", bit = if self.get_bit(square) { 1 } else { 0 })?;
            } println!();
        }
        
        writeln!(f, "\n     a b c d e f g h")?;
        write!(f, "     bitboard: {bitboard:064b}")
    }
}

impl Debug for BitBoard {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

pub struct BoardIterator {
    board: BitBoard,
    pos: usize
}

impl Iterator for BoardIterator {
    type Item = usize;

    fn next(&mut self) -> Option::<Self::Item> {
        while self.pos < BitBoard::SIZE {
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
    pub const fn init_board(&self) -> BitBoard {
        match self {
            Self::WhitePawns   => BitBoard::white_pawns(),
            Self::BlackPawns   => BitBoard::black_pawns(),
            Self::WhiteRooks   => BitBoard::white_rooks(),
            Self::BlackRooks   => BitBoard::black_rooks(),
            Self::WhiteKnights => BitBoard::white_knights(),
            Self::BlackKnights => BitBoard::black_knights(),
            Self::WhiteBishops => BitBoard::white_bishops(),
            Self::BlackBishops => BitBoard::black_bishops(),
            Self::WhiteQueens  => BitBoard::white_queen(),
            Self::BlackQueens  => BitBoard::black_queen(),
            Self::WhiteKings   => BitBoard::white_king(),
            Self::BlackKings   => BitBoard::black_king(),
            Self::AllWhites    => BitBoard::all_whites(),
            Self::AllBlacks    => BitBoard::all_blacks(),
            _ => unreachable!()
        }
    }
}

mod mv {
    pub const CAP: usize = 128;
    pub type Vec = smallvec::SmallVec::<[super::Move; CAP]>;

    #[macro_export]
    macro_rules! mv {
        ($src: expr, $dst: expr) => { Move::new_from_index($src, $dst) };
        ($src: expr, $dst: expr, $prom: expr) => { Move::new_from_index_promotion($src, $dst, $prom) };
        (.$src: tt, .$dst: tt) => { Move::new_from_square(Square::$src, Square::$dst) };
        (.$src: tt, .$dst: tt, $prom: expr) => { Move::new_from_square_promotion(Square::$src, Square::$dst, $prom) };
    }
}

#[repr(packed)]
#[derive(Eq, Copy, Clone, Debug, PartialEq)]
pub struct Move(u16);

impl Move {
    #[inline(always)]
    pub const fn new_from_index_promotion(src: usize, dst: usize, promotion: Option::<Piece>) -> Self {
        let promotion_bits = match promotion {
            Some(p) => (p as u16) << 12,
            _ => 0
        };
        Self((src as u16) | ((dst as u16) << 6) | promotion_bits)
    }

    #[inline(always)]
    pub const fn new_from_index(src: usize, dst: usize) -> Self {
        Self::new_from_index_promotion(src, dst, None)
    }

    #[inline(always)]
    pub const fn new_from_square_promotion(src: Square, dst: Square, promotion: Option::<Piece>) -> Self {
        Self::new_from_index_promotion(src.to_bit_index(), dst.to_bit_index(), promotion)
    }

    #[inline(always)]
    pub const fn new_from_square(src: Square, dst: Square) -> Self {
        Self::new_from_index(src.to_bit_index(), dst.to_bit_index())
    }

    #[inline(always)]
    pub const fn from_square(self) -> Square {
        Square::from_index((self.0 & 0x3F) as _)
    }

    #[inline(always)]
    pub const fn from_bit_index(self) -> usize {
        (self.0 & 0x3F) as _
    }

    #[inline(always)]
    pub const fn to_square(self) -> Square {
        Square::from_index(((self.0 >> 6) & 0x3F) as _)
    }

    #[inline(always)]
    pub const fn to_bit_index(self) -> usize {
        ((self.0 >> 6) & 0x3F) as _
    }

    #[inline(always)]
    pub const fn promotion(self) -> Option::<Piece> {
        match (self.0 >> 12) & 0xF {
            0 => None,
            x @ _ => Some(Piece::from_index(x as _))
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!{
            f,
            "from: {from}, to: {to}, promotion: {prom:?}",
            from = self.from_square(),
            to = self.to_square(),
            prom = self.promotion()
        }
    }
}

// it's just easier to turn have this fn-like macro here
macro_rules! legal_moves {
    ($self: expr, $moves: ident, $color: tt) => { paste::paste! {
        $self.[<iter_ $color s>]().zip(Self::[<$color:upper S>]).flat_map(|(board, pieces)| {
            board.iter().flat_map(move |src| {
                match pieces {
                    Pieces::[<$color:camel Pawns>]   => BitBoard::[<get_ $color _pawn_moves>](src, $self.occupancy(), *$self.[<all_ $color s>]()),
                    Pieces::[<$color:camel Knights>] => BitBoard::get_knight_moves(src, *$self.[<all_ $color s>]()),
                    Pieces::[<$color:camel Bishops>] => BitBoard::get_bishop_moves(src, $self.occupancy(), *$self.[<all_ $color s>]()),
                    Pieces::[<$color:camel Rooks>]   => BitBoard::get_rook_moves(src, $self.occupancy(), *$self.[<all_ $color s>]()),
                    Pieces::[<$color:camel Queens>]  => BitBoard::get_queen_moves(src, $self.occupancy(),* $self.[<all_ $color s>]()),
                    Pieces::[<$color:camel Kings>]   => BitBoard::get_king_moves(src, *$self.[<all_ $color s>]()),
                    _ => unreachable!()
                }.iter().map(move |dst| mv![src, dst])
            })
        }).collect::<mv::Vec>()
    }}
}

#[derive(Debug)]
pub struct Board {
    boards: [BitBoard; Pieces::_RESERVED_PIECES_COUNT.to_index()],
    turn: bool, // true -> white, false -> black
}

impl Board {
    #[inline(always)]
    pub const fn new() -> Self {
        Self {
            boards: [
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
            ],
            turn: true
        }
    }

    #[inline(always)]
    pub const fn pieces(&self, pieces: Pieces) -> &BitBoard {
        &self.boards[pieces as usize]
    }

    #[inline(always)]
    pub const fn pieces_mut(&mut self, pieces: Pieces) -> &mut BitBoard {
        &mut self.boards[pieces as usize]
    }

    #[inline(always)]
    pub const fn pieces_count(&self, pieces: Pieces) -> usize {
        self.boards[pieces as usize].count()
    }

    // get piece kind at `src`th bit
    #[track_caller]
    #[inline(always)]
    pub const fn get_kind(&self, src: usize) -> Pieces {
        let mut i = 0;
        while i < Pieces::_RESERVED_PIECES_COUNT.to_index() - 2 {
            if self.boards[i].get_bit(src) {
                return Pieces::from_index(i)
            } i += 1;
        } unreachable!()
    }

    pub const WHITES: [Pieces; 6] = [
        Pieces::WhitePawns,
        Pieces::WhiteKnights,
        Pieces::WhiteBishops,
        Pieces::WhiteRooks,
        Pieces::WhiteQueens,
        Pieces::WhiteKings,
    ];

    #[inline(always)]
    pub fn iter_whites(&self) -> impl Iterator::<Item = BitBoard> + '_ {
        Self::WHITES.into_iter().map(|pieces| *self.pieces(pieces)).into_iter()
    }

    pub const BLACKS: [Pieces; 6] = [
        Pieces::BlackPawns,
        Pieces::BlackKnights,
        Pieces::BlackBishops,
        Pieces::BlackRooks,
        Pieces::BlackQueens,
        Pieces::BlackKings,
    ];

    #[inline(always)]
    pub fn iter_blacks(&self) -> impl Iterator::<Item = BitBoard> + '_ {
        Self::BLACKS.into_iter().map(|pieces| *self.pieces(pieces)).into_iter()
    }

    #[inline(always)]
    pub fn iter_pair(&self) -> impl Iterator::<Item = (BitBoard, BitBoard)> + '_ {
        self.iter_whites().zip(self.iter_blacks()).into_iter()
    }

    #[inline(always)]
    pub fn iter(&self) -> impl Iterator::<Item = BitBoard> + '_ {
        self.boards.into_iter().take(Pieces::_RESERVED_PIECES_COUNT.to_index() - 2)
    }

    for_all_pieces!(pieces -> &BitBoard);
    for_all_pieces!(mut _mut pieces_mut -> &mut BitBoard);
    for_all_pieces!(_count pieces_count -> usize);

    #[inline(always)]
    pub const fn phase(&self) -> _Score {
        const KNIGHT_WEIGHT: _Score = 1;
        const BISHOP_WEIGHT: _Score = 1;
        const ROOK_WEIGHT:   _Score = 2;
        const QUEEN_WEIGHT:  _Score = 4;

        const MAX_PHASE: _Score = 2 * (KNIGHT_WEIGHT +
                                       BISHOP_WEIGHT +
                                       ROOK_WEIGHT)  + QUEEN_WEIGHT;

        let ret = self.white_knights_count() as _Score * KNIGHT_WEIGHT +
                  self.black_knights_count() as _Score * KNIGHT_WEIGHT +
                  self.white_bishops_count() as _Score * BISHOP_WEIGHT +
                  self.black_bishops_count() as _Score * BISHOP_WEIGHT +
                  self.white_rooks_count()   as _Score * ROOK_WEIGHT   +
                  self.black_rooks_count()   as _Score * ROOK_WEIGHT   +
                  self.white_queens_count()  as _Score * QUEEN_WEIGHT  +
                  self.black_queens_count()  as _Score * QUEEN_WEIGHT;

        if ret < MAX_PHASE { ret } else { MAX_PHASE }
    }

    #[inline(always)]
    pub const fn make_move_index(&mut self, src: usize, dst: usize) {
        let kind = self.get_kind(src);
        let board = self.pieces_mut(kind);

        board.clear_bit(src);
        board.set_bit(dst, true);

        if kind.is_white() {
            self.all_whites_mut().clear_bit(src);
            self.all_whites_mut().set_bit(dst, true);
        } else {
            self.all_blacks_mut().clear_bit(src);
            self.all_blacks_mut().set_bit(dst, true);
        }

        self.turn = !self.turn;
    }

    #[inline(always)]
    pub const fn make_move(&mut self, mv: Move) {
        self.make_move_index(mv.from_bit_index(), mv.to_bit_index())
    }

    #[inline(always)]
    pub const fn occupancy(&self) -> BitBoard {
        BitBoard(self.all_whites().0 | self.all_blacks().0)
    }

    #[inline(always)]
    pub const fn empty(&self) -> BitBoard {
        BitBoard(!(self.all_whites().0 | self.all_blacks().0))
    }

    #[inline]
    pub fn legal_moves(&self) -> mv::Vec {
        if self.turn {
            legal_moves!(self, moves, white)
        } else {
            legal_moves!(self, moves, black)
        }
    }

    #[inline]
    pub fn evaluate(&self) -> Score {
        const fn next(board: &mut BitBoard) -> usize {
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

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut char_board = ['.'; 64];
        for (idx, board) in self.iter().enumerate() {
            let piece_char = Pieces::from_index(idx).to_char();

            for pos in board.iter() {
                char_board[pos] = piece_char;
            }
        }

        writeln!(f, " +-----------------+")?;
        for rank in (0..8).rev() {
            write!(f, " |")?;
            for file in 0..8 {
                let pos = ((BitBoard::HEIGHT - 1 - rank) * BitBoard::WIDTH) + file;
                write!(f, " {0}", char_board[pos])?;
            }
            writeln!(f, " | {0}", rank + 1)?;
        }
        writeln!(f, " +-----------------+")?;
        writeln!(f, "   a b c d e f g h")?;
        write!(f, "turn: {color}", color = if self.turn { "white" } else { "black" })
    }
}

fn main() {
    let mut board = Board::new();
    board.make_move(mv![.D2, .D4]);
    board.make_move(mv![.E7, .E5]);
    board.make_move(mv![.F2, .F4]);
    board.make_move(mv![.D8, .B3]);
    println!("{board}");
    println!("evaluation: {}", board.evaluate());
    board.legal_moves().iter().for_each(|mov| println!("{mov}"));
    // println!("{boards}");

    // println!("{}", BitBoard(WHITE_PAWN_MOVES[A2.to_index()]));
    println!("{}", BitBoard::get_white_pawn_moves_square(A2, *board.pieces(Pieces::BlackQueens), BitBoard::all_whites()));
    // println!("{}", BitBoard::get_queen_moves_square(A2, BitBoard::all_blacks(), BitBoard::all_whites()));
}
