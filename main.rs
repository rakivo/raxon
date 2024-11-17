use std::fmt::{Debug, Display};

mod uci;
mod psqt;
mod magic;
#[macro_use]
mod macros;

use uci::*;
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

pub const ALL_PIECES_COUNT: usize = Pieces::_RESERVED_PIECES_COUNT.to_index() - 2;

#[inline(always)]
pub const fn fen_byte_to_map_index(b: u8) -> usize {
    (b - b'B') as usize
}

pub const FEN_MAP_SIZE: usize = fen_byte_to_map_index(b'r') + 1;

pub const FEN_BB_MAP: [usize; FEN_MAP_SIZE] = const {
    use Pieces::*;
    let mut piece_map = [0; FEN_MAP_SIZE];
    piece_map[fen_byte_to_map_index(b'P')] = WhitePawns.to_index();
    piece_map[fen_byte_to_map_index(b'p')] = BlackPawns.to_index();
    piece_map[fen_byte_to_map_index(b'N')] = WhiteKnights.to_index();
    piece_map[fen_byte_to_map_index(b'n')] = BlackKnights.to_index();
    piece_map[fen_byte_to_map_index(b'B')] = WhiteBishops.to_index();
    piece_map[fen_byte_to_map_index(b'b')] = BlackBishops.to_index();
    piece_map[fen_byte_to_map_index(b'R')] = WhiteRooks.to_index();
    piece_map[fen_byte_to_map_index(b'r')] = BlackRooks.to_index();
    piece_map[fen_byte_to_map_index(b'Q')] = WhiteQueens.to_index();
    piece_map[fen_byte_to_map_index(b'q')] = BlackQueens.to_index();
    piece_map[fen_byte_to_map_index(b'K')] = WhiteKings.to_index();
    piece_map[fen_byte_to_map_index(b'k')] = BlackKings.to_index();
    piece_map
};

pub type Turn = bool;

pub type _Score = i16;

#[repr(packed)]
#[derive(Copy, Clone)]
pub struct Castling(u8);

impl Castling {
    pub const WKS: u8 = 1 << 0;
    pub const WQS: u8 = 1 << 1;
    pub const BKS: u8 = 1 << 2;
    pub const BQS: u8 = 1 << 3;
    pub const WKS_AND_WQS: u8 = Self::WKS | Self::WQS;
    pub const BKS_AND_BQS: u8 = Self::BKS | Self::BQS;

    #[inline(always)]
    pub const fn new() -> Self {
        Self(0)
    }

    #[inline(always)]
    pub const fn no_castling() -> Self {
        Self::new()
    }

    #[inline(always)]
    pub const fn to_castling_variant(&self) -> CastlingVariant {
        use CastlingVariant::*;
        if self.0 == Self::WKS | Self::WQS {
            WhiteKingAndQueenSide
        } else if self.0 == Self::WKS {
            WhiteKingSide
        } else if self.0 == Self::WQS {
            WhiteQueenSide
        } else if self.0 == Self::BKS | Self::BQS {
            BlackKingAndQueenSide
        } else if self.0 == Self::BKS {
            BlackKingSide
        } else if self.0 == Self::BQS {
            BlackQueenSide
        } else {
            NoCastling
        }
    }

    #[inline(always)]
    pub const fn is(&self, castling: CastlingVariant) -> bool {
        self.0 == castling.to_castling().0
    }

    #[inline(always)]
    pub const fn white_king_side() -> Self {
        Self(Self::WKS)
    }

    #[inline(always)]
    pub const fn white_queen_side() -> Self {
        Self(Self::WQS)
    }

    #[inline(always)]
    pub const fn white_king_and_queen_side() -> Self {
        Self(Self::white_king_side().0 | Self::white_queen_side().0)
    }

    #[inline(always)]
    pub const fn black_king_side() -> Self {
        Self(Self::BKS)
    }

    #[inline(always)]
    pub const fn black_queen_side() -> Self {
        Self(Self::BQS)
    }

    #[inline(always)]
    pub const fn black_king_and_queen_side() -> Self {
        Self(Self::black_king_side().0 | Self::black_queen_side().0)
    }

    #[inline(always)]
    pub const fn set(&mut self, var: CastlingVariant) {
        use CastlingVariant::*;
        *self = match var {
            WhiteKingSide => Self::white_king_side(),
            WhiteQueenSide => Self::white_queen_side(),
            BlackKingSide => Self::black_king_side(),
            BlackQueenSide => Self::black_queen_side(),
            WhiteKingAndQueenSide => Self::white_king_and_queen_side(),
            BlackKingAndQueenSide => Self::black_king_and_queen_side(),
            NoCastling => Self::no_castling(),
            _ => unreachable!()
        };
    }

    #[inline(always)]
    pub const fn from_var(var: CastlingVariant) -> Self {
        let mut castling = Self::new();
        castling.set(var);
        castling
    }

    #[inline]
    pub const fn from_fen(castling: &str) -> Self {
        let mut i = 0;
        let bytes = castling.as_bytes();

        let mut castling = 0;
        while i < bytes.len() {
            castling |= match bytes[i] {
                b'K' => Self::white_king_side().0,
                b'Q' => Self::white_queen_side().0,
                b'k' => Self::black_king_side().0,
                b'q' => Self::black_queen_side().0,
                _ => Self::new().0
            }; i += 1;
        } Self(castling)
    }
}

impl Display for Castling {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Castling::WKS => write!(f, "white king-side castling"),
            Castling::WQS => write!(f, "white queen-side castling"),
            Castling::BKS => write!(f, "black king-side castling"),
            Castling::BQS => write!(f, "black queen-side castling"),
            Castling::WKS_AND_WQS => write!(f, "white king and queen side castling"),
            Castling::BKS_AND_BQS => write!(f, "black king and queen side castling"),
            _ => write!(f, "no castling")
        }
    }
}

impl Debug for Castling {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

#[repr(u8)]
pub enum CastlingVariant {
    WhiteKingSide         = 0,
    WhiteQueenSide        = 1,
    WhiteKingAndQueenSide = 2,

    BlackKingSide         = 3,
    BlackQueenSide        = 4,
    BlackKingAndQueenSide = 5,

    NoCastling = 6,

    #[allow(non_camel_case_types)]
    _RESERVED_CASTLING_VARIANT_COUNT = 7
}

impl CastlingVariant {
    gen_from_to_index!{u8, if idx < Self::_RESERVED_CASTLING_VARIANT_COUNT.to_index(); "invalid idx"}

    #[inline(always)]
    pub const fn to_castling(&self) -> Castling {
        use CastlingVariant::*;
        match *self {
            WhiteKingSide => Castling::white_king_side(),
            WhiteQueenSide => Castling::white_queen_side(),
            WhiteKingAndQueenSide => Castling::white_king_and_queen_side(),

            BlackKingSide => Castling::black_king_side(),
            BlackQueenSide => Castling::black_queen_side(),
            BlackKingAndQueenSide => Castling::black_king_and_queen_side(),

            NoCastling => Castling::no_castling(),

            _ => unreachable!()
        }
    }
}

impl Display for CastlingVariant {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.to_castling(), f)
    }
}

impl Debug for CastlingVariant {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.to_castling(), f)
    }
}

#[repr(packed)]
#[derive(Copy, Clone, Debug)]
pub struct Score(pub _Score, pub _Score);

impl Display for Score {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (w, b) = (self.0, self.1);
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
            Some(Piece::from_index(pos))
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
    const fn from_fen(square: &str) -> Self {
        let bytes = square.as_bytes();
        #[cfg(debug_assertions)]
        if bytes.len() != 2 {
            panic!("invalid square data: {square}")
        }
        let col = (bytes[0] - b'a') as usize;
        let row = (bytes[1] - b'1') as usize;
        Self::from_index(FLIP[(row * 8) + col])
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
    pub const fn flip_rank_usize(rank: usize) -> usize {
        rank ^ A8.to_index()
    }

    // Swap A1 <-> A8
    #[inline(always)]
    pub const fn flip_rank(&self) -> usize {
        self.to_index() ^ A8.to_index()
    }

    #[inline(always)]
    pub const fn flip_rank_file(file: usize) -> usize {
        file ^ H1.to_index()
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
        Debug::fmt(&self, f)
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
    pub const fn get_white_pawn_moves(square: usize, enemy: Self, friendly: Self) -> Self {
        let square = FLIP[square];
        Self((WHITE_PAWN_MOVES[square] & !enemy.0 & !friendly.0) | (WHITE_PAWN_ATTACKS[square] & enemy.0))
    }

    #[inline(always)]
    pub const fn get_black_pawn_moves(square: usize, enemy: Self, friendly: Self) -> Self {
        let square = FLIP[square];
        Self((BLACK_PAWN_MOVES[square] & !enemy.0 & !friendly.0 & !enemy.0) | (BLACK_PAWN_ATTACKS[square] & enemy.0))
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
        Self::get_white_pawn_moves(pos.to_bit_index() as _, occupancy, friendly)
    }

    #[inline(always)]
    pub const fn get_black_pawn_moves_square(pos: Square, occupancy: Self, friendly: Self) -> Self {
        Self::get_black_pawn_moves(pos.to_bit_index() as _, occupancy, friendly)
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
    pub const fn iter(&self) -> BitBoardIterator {
        BitBoardIterator {
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

pub struct BitBoardIterator {
    board: BitBoard,
    pos: usize
}

impl Iterator for BitBoardIterator {
    type Item = usize;

    #[inline]
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
#[derive(Copy, Clone, Debug, PartialEq)]
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

    // might panic, be careful :D
    #[inline(always)]
    pub const fn invert_color(&self) -> Self {
        if self.is_white() {
            Self::from_index(self.to_index() + 1)
        } else {
            Self::from_index(self.to_index() - 1)
        }
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
        ($src: expr, $dst: expr) => { Move::from_index($src, $dst) };
        ($src: expr, $dst: expr, p.$prom: expr) => { Move::from_index_promotion($src, $dst, p.$prom) };
        ($src: expr, $dst: expr, c.$capture: expr) => { Move::from_index_full($src, $dst, None, Some($capture), None) };

        (.$src: tt, .$dst: tt) => { Move::new_src_square(Square::$src, Square::$dst) };
        (.$src: tt, .$dst: tt, p.$prom: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, Some($prom), None, None) };
        (.$src: tt, .$dst: tt, c.$capture: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, None, Some($capture), None) };
        (.$src: tt, .$dst: tt, p.$prom: expr, c.$capture: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, Some($prom), Some($capture), None) };
        (.$src: tt, .$dst: tt, p.$prom: expr, c.$capture: expr, cas.$castling: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, Some($prom), Some($capture), Some($castling)) };
        (.$src: tt, .$dst: tt, c.$capture: expr, cas.$castling: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, None, Some($capture), Some($castling)) };
        (.$src: tt, .$dst: tt, cas.$castling: expr, c.$capture: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, None, Some($capture), Some($castling)) };
        (.$src: tt, .$dst: tt, p.$prom: expr, cas.$capture: expr, c.$castling: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, Some($prom), Some($capture), Some($castling)) };
        (.$src: tt, .$dst: tt, cas.$capture: expr, p.$prom: expr, c.$castling: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, Some($prom), Some($capture), Some($castling)) };
        (.$src: tt, .$dst: tt, cas.$capture: expr, c.$castling: expr, p.$prom: expr) => { Move::new_src_square_full(Square::$src, Square::$dst, Some($prom), Some($capture), Some($castling)) };
        (.$src: tt, .$dst: tt, cas.$castling: expr) => { Move::new_src_square_promotion(Square::$src, Square::$dst, $prom) };
    }
}

#[repr(packed)]
#[derive(Eq, Copy, Clone, Debug, PartialEq)]
pub struct Move(u64); // Use u64 for more flexibility if needed

impl Move {
    const CASTLING_MASK: u64 = 0xF << 16;  // 4 bits for castling (16-19)
    const CAPTURE_MASK: u64 = 1 << 20;     // 1 bit for capture (20)
    const PROMOTION_MASK: u64 = 0xF << 12; // 4 bits for promotion (12-15)
    
    pub fn from_algebraic_notation(an: &str) -> Self {
        println!("from an: {an}");

        let mut move_value = 0u64;

        // Handle castling moves (e.g., "O-O", "O-O-O")
        if an == "O-O" || an == "O-O-O" {
            let castling = if an == "O-O" { CastlingVariant::WhiteKingSide } else { CastlingVariant::WhiteQueenSide };
            return Self::from_castling_variant(castling);
        }

        fn trim_piece_at_the_left(s: &str) -> &str {
            const PIECE_LIST: &[u8] = &[b'Q', b'R', b'B', b'N', b'K', b'q', b'r', b'b', b'n', b'k'];
            let bytes = s.as_bytes();
            if PIECE_LIST.contains(&bytes[0]) && PIECE_LIST.contains(&bytes[1]) {
                &s[1..]
            } else {
                &s[..]
            }
        }

        let an = trim_piece_at_the_left(an);
        let (notation, capture) = if let Some(capture_pos) = an.find('x') {
            let src_square = &an[..capture_pos];
            let dst_square = &an[capture_pos + 1..]; // e.g., "f5"
            (src_square.to_string() + dst_square, Some(dst_square))
        } else {
            (an.to_string(), None)
        };

        let (notation, promotion_char) = if let Some(prom_char) = notation.chars().last() {
            if ['Q', 'R', 'B', 'N'].contains(&prom_char) {
                let move_notation = &notation[..notation.len() - 1]; // Remove promotion char
                (move_notation.to_string(), Some(prom_char))
            } else {
                (notation, None)
            }
        } else {
            (notation, None)
        };

        // Handle regular moves (e.g., "e2e4", "Nf3", "e7e8")
        let src_square = Self::algebraic_to_square(&notation[0..2]);
        let dst_square = Self::algebraic_to_square(&notation[2..4]);

        // Apply the move (in terms of source and destination squares)
        move_value |= (src_square.to_bit_index() as u64) | ((dst_square.to_bit_index() as u64) << 6);

        // Handle promotion (only if there's a valid promotion character)
        if let Some(promoted_piece) = promotion_char {
            let promotion_piece = match promoted_piece {
                'N' => Pieces::WhiteKnights,
                'B' => Pieces::WhiteBishops,
                'R' => Pieces::WhiteRooks,
                'Q' => Pieces::WhiteQueens,
                _ => panic!("Invalid promoted piece: {}", promoted_piece),
            };
            move_value |= (promotion_piece.to_index() as u64) << 12;
        } else {
            move_value |= (Pieces::_RESERVED_PIECES_COUNT.to_index() as u64) << 12;
        }

        // Handle capture (if there was a capture)
        if capture.is_some() {
            move_value |= Self::CAPTURE_MASK;
        }

        Self(move_value)
    }

    pub fn algebraic_to_square(sq: &str) -> Square {
        // The file (a-h) maps to 0-7, and the rank (1-8) maps to 0-7.
        let file = match sq.chars().next().unwrap() {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            'e' => 4,
            'f' => 5,
            'g' => 6,
            'h' => 7,
            _ => panic!("Invalid file in algebraic notation: {sq}"),
        };
        
        let rank = sq.chars().nth(1).unwrap().to_digit(10).unwrap() as usize - 1;

        // Convert to the Square enum using the file and rank values
        Square::from_index(rank * 8 + file)
    }

    #[inline(always)]
    pub fn from_castling(castling: Castling) -> Self {
        // Castling value will be stored as signed integer
        Self((castling.0 as u64) << 16)
    }

    #[inline(always)]
    pub fn from_castling_variant(castling_variant: CastlingVariant) -> Self {
        // Castling value will be stored as signed integer
        Self((castling_variant.to_castling().0 as u64) << 16)
    }

    #[inline(always)]
    pub fn from_index_full(src: usize, dst: usize, promotion: Option<Pieces>, capture: Option<Pieces>, castling: Option<CastlingVariant>) -> Self {
        #[cfg(debug_assertions)]
        if src > 63 || dst > 63 {
            panic!("{src} or {dst} is too big to be packed")
        }

        let mut move_value = (src as u64) | ((dst as u64) << 6);

        let promotion = promotion.unwrap_or(Pieces::_RESERVED_PIECES_COUNT);
        move_value |= (promotion as u64) << 12;

        // Handle capture
        if capture.is_some() {
            move_value |= Self::CAPTURE_MASK;
        }

        let castling_val = castling.map(|cas| cas.to_castling().0).unwrap_or(CastlingVariant::_RESERVED_CASTLING_VARIANT_COUNT as u8);
        move_value |= (castling_val as u64) << 16;

        Self(move_value)
    }

    #[inline(always)]
    pub fn from_index(src: usize, dst: usize) -> Self {
        Self::from_index_full(src, dst, None, None, None)
    }

    #[inline(always)]
    pub fn from_capture(src: usize, dst: usize, capture: Pieces) -> Self {
        Self::from_index_full(src, dst, None, Some(capture), None)
    }

    #[inline(always)]
    pub fn new_src_square_full(src: Square, dst: Square, promotion: Option<Pieces>, capture: Option<Pieces>, castling: Option<CastlingVariant>) -> Self {
        Self::from_index_full(src.to_bit_index(), dst.to_bit_index(), promotion, capture, castling)
    }

    #[inline(always)]
    pub fn new_src_square_promotion(src: Square, dst: Square, promotion: Option<Pieces>) -> Self {
        Self::from_index_full(src.to_bit_index(), dst.to_bit_index(), promotion, None, None)
    }

    #[inline(always)]
    pub fn new_src_square(src: Square, dst: Square) -> Self {
        Self::from_index(src.to_bit_index(), dst.to_bit_index())
    }

    #[inline(always)]
    pub const fn src_square(self) -> Square {
        Square::from_index(FLIP[(self.0 & 0x3F) as usize])
    }

    #[inline(always)]
    pub fn dst_square(self) -> Square {
        Square::from_index(FLIP[((self.0 >> 6) & 0x3F) as usize])
    }

    pub fn to_algebraic_notation(self) -> String {
        if let Some(castling) = self.castling() {
            match castling {
                CastlingVariant::WhiteKingSide | CastlingVariant::BlackKingSide => return "O-O".to_string(),
                CastlingVariant::WhiteQueenSide | CastlingVariant::BlackQueenSide => return "O-O-O".to_string(),
                _ => {}
            }
        }

        let src = self.src_square();
        let dst = self.dst_square();
        println!("converting src: {src} square and dst: {dst} square to algebraic notation");

        let notation = if self.capture().is_some() {
            format!("{:?}x{:?}", src, dst).to_lowercase()
        } else {
            format!("{:?}{:?}", src, dst).to_lowercase()
        };
        
        if let Some(promoted_piece) = self.promotion() {
            if promoted_piece == Pieces::WhitePawns || promoted_piece == Pieces::BlackPawns {
                return notation
            }
            let promoted_char = match promoted_piece {
                Pieces::WhiteKnights | Pieces::BlackKnights => 'N',
                Pieces::WhiteBishops | Pieces::BlackBishops => 'B',
                Pieces::WhiteRooks | Pieces::BlackRooks     => 'R',
                Pieces::WhiteQueens | Pieces::BlackQueens   => 'Q',
                _ => panic!("Unexpected piece for promotion"),
            };
            format!("{notation}={promoted_char}")
        } else {
            notation
        }
    }

    #[inline(always)]
    pub const fn from_bit_index(self) -> usize {
        (self.0 & 0x3F) as _
    }

    #[inline(always)]
    pub const fn to_bit_index(self) -> usize {
        ((self.0 >> 6) & 0x3F) as _
    }

    #[inline(always)]
    pub fn promotion(self) -> Option<Pieces> {
        let promotion_bits = (self.0 >> 12) & 0xF;
        if promotion_bits >= Pieces::_RESERVED_PIECES_COUNT.to_index() as u64 - 1 {
            None
        } else {
            Some(Pieces::from_index(promotion_bits as usize))
        }
    }

    #[inline(always)]
    pub const fn capture(self) -> Option<Pieces> {
        if self.0 & Self::CAPTURE_MASK != 0 {
            Some(Pieces::from_index(((self.0 >> 21) & 0xF) as usize + 1)) // Capture piece encoded from bits 21 to 24
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn castling(self) -> Option<CastlingVariant> {
        match self.0 >> 16 & 0xF {
            0 => None,
            x => Some(CastlingVariant::from_index(x as usize - 1))
        }
    }
}

impl Display for Move {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!{
            f,
            "from: {from}, to: {to}, promotion: {prom:?}, capture: {cap:?}, castling: {cas:?}",
            from = self.src_square(),
            to = self.dst_square(),
            prom = self.promotion(),
            cap = self.capture(),
            cas = self.castling()
        }
    }
}

// it's just easier to have this fn-like macro here
macro_rules! legal_moves {
    ($self: expr, $moves: ident, $enemy: expr, $color: tt) => { paste::paste! {
        $self.[<iter_ $color s>]().zip(Self::[<$color:upper S>]).flat_map(|(board, pieces)| {
            board.iter().flat_map(move |src| {
                match pieces {
                    Pieces::[<$color:camel Pawns>]   => BitBoard::[<get_ $color _pawn_moves>](src, $self.enemy(), $self.friendly()),
                    Pieces::[<$color:camel Knights>] => BitBoard::get_knight_moves(src, $self.friendly()),
                    Pieces::[<$color:camel Bishops>] => BitBoard::get_bishop_moves(src, $self.occupancy(), $self.friendly()),
                    Pieces::[<$color:camel Rooks>]   => BitBoard::get_rook_moves(src, $self.occupancy(), $self.friendly()),
                    Pieces::[<$color:camel Queens>]  => BitBoard::get_queen_moves(src, $self.occupancy(), $self.friendly()),
                    Pieces::[<$color:camel Kings>]   => BitBoard::get_king_moves(src, $self.friendly()),
                    _ => unreachable!()
                }.iter().map(move |dst| {
                    if $enemy.get_bit(dst) {
                        mv![src, dst, c.Pieces::[<$color:camel Pawns>]] // just for now
                    } else {
                        mv![src, dst]
                    }
                })
            })
        }).collect::<mv::Vec>()
    }};

    (.$self: expr, $moves: ident, $color: tt) => { paste::paste! {
        $self.[<iter_ $color s>]().zip(Self::[<$color:upper S>]).flat_map(|(board, pieces)| {
            board.iter().flat_map(move |src| {
                match pieces {
                    Pieces::[<$color:camel Pawns>]   => BitBoard::[<get_ $color _pawn_moves>](src, $self.occupancy(), $self.friendly()),
                    Pieces::[<$color:camel Knights>] => BitBoard::get_knight_moves(src, $self.friendly()),
                    Pieces::[<$color:camel Bishops>] => BitBoard::get_bishop_moves(src, $self.occupancy(), $self.friendly()),
                    Pieces::[<$color:camel Rooks>]   => BitBoard::get_rook_moves(src, $self.occupancy(), $self.friendly()),
                    Pieces::[<$color:camel Queens>]  => BitBoard::get_queen_moves(src, $self.occupancy(), $self.friendly()),
                    Pieces::[<$color:camel Kings>]   => BitBoard::get_king_moves(src, $self.friendly()),
                    _ => unreachable!()
                }.iter().map(move |dst| {
                    let mut zelf = *$self;
                    zelf.make_move(mv![src, dst]);
                    zelf
                })
            })
        }).collect::<board::Vec>()
    }}
}

mod board {
    pub const CAP: usize = 128;
    pub type Vec = smallvec::SmallVec::<[super::Board; CAP]>;
}

#[derive(Copy, Clone, Debug)]
pub struct Board {
    turn: Turn, // true -> white, false -> black
    castling_rights: Castling,
    en_passant: Option::<Square>,
    boards: [BitBoard; Pieces::_RESERVED_PIECES_COUNT.to_index()]
}

impl Board {
    pub const EMPTY_BOARDS: [BitBoard; Pieces::_RESERVED_PIECES_COUNT.to_index()] = [
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new(),
        BitBoard::new()
    ];

    pub const BOARDS: [BitBoard; Pieces::_RESERVED_PIECES_COUNT.to_index()] = [
        Pieces::WhitePawns.init_board(),   // 0
        Pieces::BlackPawns.init_board(),   // 1
        Pieces::WhiteKnights.init_board(), // 2
        Pieces::BlackKnights.init_board(), // 3
        Pieces::WhiteBishops.init_board(), // 4
        Pieces::BlackBishops.init_board(), // 5
        Pieces::WhiteRooks.init_board(),   // 6
        Pieces::BlackRooks.init_board(),   // 7
        Pieces::WhiteQueens.init_board(),  // 8
        Pieces::BlackQueens.init_board(),  // 9
        Pieces::WhiteKings.init_board(),   // 10
        Pieces::BlackKings.init_board(),   // 11
        Pieces::AllWhites.init_board(),    // 12
        Pieces::AllBlacks.init_board(),    // 13
    ];

    #[inline(always)]
    pub const fn new() -> Self {
        Self {
            boards: Self::BOARDS,
            en_passant: None,
            castling_rights: Castling::new(),
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
    pub fn get_kind(&self, src: usize) -> Pieces {
        #[cfg(not(debug_assertions))]
        unsafe { self.get_kind_option(src).unwrap_unchecked() }

        #[cfg(debug_assertions)]
        { self.get_kind_option(src).unwrap() }
    }

    // get piece kind at `src`th bit
    #[track_caller]
    #[inline(always)]
    pub fn get_kind_option(&self, src: usize) -> Option::<Pieces> {
        let mut i = 0;
        while i < ALL_PIECES_COUNT {
            if self.boards[i].get_bit(src) {
                return Some(Pieces::from_index(i))
            } i += 1;
        } None
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
        self.boards.into_iter().take(ALL_PIECES_COUNT)
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
    pub fn make_move_index(&mut self, src: usize, dst: usize) {
        let src_piece_kind = self.get_kind(src);
        
        let board = self.pieces_mut(src_piece_kind);

        board.clear_bit(src);
        board.set_bit(dst, true);

        if src_piece_kind.is_white() {
            self.all_whites_mut().clear_bit(src);
            self.all_whites_mut().set_bit(dst, true);
            if let Some(dst_piece_kind) = self.get_kind_option(dst) {
                self.all_blacks_mut().clear_bit(dst);
                self.pieces_mut(dst_piece_kind.invert_color()).clear_bit(dst);
            }
        } else {
            if let Some(dst_piece_kind) = self.get_kind_option(dst) {
                self.all_whites_mut().clear_bit(dst);
                self.pieces_mut(dst_piece_kind.invert_color()).clear_bit(dst);
            }
            self.all_blacks_mut().clear_bit(src);
            self.all_blacks_mut().set_bit(dst, true);
        }

        self.turn = !self.turn;
    }

    #[inline(always)]
    pub fn make_move(&mut self, mv: Move) {
        self.make_move_index(mv.from_bit_index(), mv.to_bit_index())
    }

    #[inline(always)]
    pub const fn friendly(&self) -> BitBoard {
        if self.turn { *self.all_whites() } else { *self.all_blacks() }
    }

    #[inline(always)]
    pub const fn enemy(&self) -> BitBoard {
        if self.turn { *self.all_blacks() } else { *self.all_whites() }
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
            legal_moves!(self, moves, self.all_blacks(), white)
        } else {
            legal_moves!(self, moves, self.all_whites(), black)
        }
    }

    #[inline]
    pub fn legal_moves_boards(&self) -> board::Vec {
        if self.turn {
            legal_moves!(.self, moves, white)
        } else {
            legal_moves!(.self, moves, black)
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

    pub fn fen_set(&mut self, fen: &str) {
        let parts = fen.split_whitespace().collect::<Vec::<_>>();

        if parts.is_empty() {
            panic!("invalid fen string: {fen}");
        }

        let (active_color, castling_rights, en_passant) = if parts.len() >= 2 {
            (parts[1], parts[2], *parts.get(3).unwrap_or(&"-"))
        } else {
            ("w", "-", "-")
        };

        let boards = parts[0].split('/').fold({
            (0, Self::EMPTY_BOARDS)
        }, |(row, mut boards), line| {
            line.bytes().fold(0, |col, b| {
                if (b'0'..=b'9').contains(&b) {
                    col + (b - b'0') as usize
                } else {
                    let idx = FEN_BB_MAP[fen_byte_to_map_index(b)];
                    let pos = row * BitBoard::WIDTH + col;
                    boards[idx].set_bit(pos as _, true);
                    if (b'B'..=b'R').contains(&b) {
                        let all_whites_idx = const { Pieces::AllWhites as usize };
                        boards[all_whites_idx].set_bit(pos as _, true);
                    } else {
                        let all_blacks_idx = const { Pieces::AllBlacks as usize };
                        boards[all_blacks_idx].set_bit(pos as _, true);
                    }
                    col + 1
                }
            }); (row + 1, boards)
        }).1;

        self.boards = boards;
        self.turn = active_color.eq_ignore_ascii_case("w");
        self.castling_rights = Castling::from_fen(castling_rights);
        self.en_passant = en_passant.ne("-").then(|| Square::from_fen(en_passant));
    }

    #[inline(always)]
    pub fn from_fen(fen: &str) -> Self {
        let mut board = Self::new();
        board.fen_set(fen);
        board
    }

    #[inline(always)]
    pub const fn reset_positions(&mut self) -> &mut Self {
        *self = Self::new();
        self
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
        writeln!(f, "turn: {color}", color = if self.turn { "white" } else { "black" })?;
        writeln!(f, "castling rights: {cr}", cr = self.castling_rights)?;
        if let Some(ep) = self.en_passant {
            write!(f, "en passant: {ep}")
        } else {
            write!(f, "en passant: [none]")
        }
    }
}

fn main() {
    // let fen = "rnbqkbnr/1ppppppp/p7/8/4PP2/8/PPPP2PP/RNBQKBNR w - - 0 1";
    // let fen = "rnbqkbnr/1ppppppp/p7/4p3/4PP2/8/PPPP2PP/RNBQKBNR w - - 0 1";
    // let fen = "rnbqkNnr/4P3/3PP3/5P1p/2p3pP/pp6/8/RNBQKB1R w KQkq - 0 21";
    // let fen = "rnbqkbnr/8/8/1Npppppp/PpPPPPPP/RR1QBN1B/8/4K3 b kq - 0 18";
    // let board = Board::from_fen(fen);
    // println!("{board}");

    let mut uci = UCI::new().unwrap();
    uci.start();

    // println!("{}", Move::from_algebraic_notation("Ng1f3"));

    // let mv = mv![.D2, .E3, p.Pieces::WhitePawns];
    // let mv = mv![.D2, .E3, c.Pieces::BlackPawns];
    // println!("{mv}");
    // println!("{}", mv.to_algebraic_notation());

    // let mut board = Board::new();
    // board.make_move(mv![.D2, .D4]);
    // board.make_move(mv![.E7, .E5]);
    // board.make_move(mv![.F2, .F4]);
    // board.make_move(mv![.D8, .B3]);
    // println!("{board}");
    // // println!("evaluation: {}", board.evaluate());
    // // println!("{}", BitBoard::get_white_pawn_moves_square(D4, board.occupancy(), board.friendly()));
    // board.legal_moves().iter().for_each(|mv| println!("{src} {dst} {cap:?}", src = mv.src_square(), dst = mv.dst_square(), cap = mv.capture()));
    // board.legal_moves_boards().iter().for_each(|board| println!("{board}"));
    // // println!("{boards}")

    // println!("{}", D4.to_index());
    // println!("{}", BitBoard(WHITE_PAWN_MOVES[D4.to_index()]));
    // let sq = E6;
    // let friendly = *board.all_whites();
    // let enemy = *board.all_blacks();
    // 110001
    // 001110
    // println!("{}", BitBoard(enemy.0));
    // println!("{}", BitBoard(!enemy.0));
    // println!("{}", BitBoard((WHITE_PAWN_MOVES[FLIP[sq.to_bit_index()]] & !enemy.0 & !friendly.0) | (WHITE_PAWN_ATTACKS[FLIP[sq.to_bit_index()]] & enemy.0)));
    // println!("{}", BitBoard::get_white_pawn_moves_square(sq, enemy, friendly));
    // println!("{}", BitBoard::get_white_pawn_moves_square(A4, *board.pieces(Pieces::BlackQueens), BitBoard::all_whites()));
    // println!("{}", BitBoard::get_queen_moves_square(A2, BiBtoard::all_blacks(), BitBoard::all_whites()));
}
