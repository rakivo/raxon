/* =======================================================================
Rustic is a chess playing engine.
Copyright (C) 2019-2024, Marcel Vanthoor
https://rustic-chess.org/

Rustic is written in the Rust programming language. It is an original
work, not derived from any engine that came before it. However, it does
use a lot of concepts which are well-known and are in use by most if not
all classical alpha/beta-based chess engines.

Rustic is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License version 3 as published by
the Free Software Foundation.

Rustic is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with this program.  If not, see <http://www.gnu.org/licenses/>.
======================================================================= */

// This file implements Piece Square Tables (PSQT) for each piece type. The
// PSQT's are written from White's point of view, as if looking at a chess
// diagram, with A1 on the lower left corner.

use crate::{Board, Piece, _Score};

type Psqt = [_Score; Board::SIZE];

#[rustfmt::skip]
const KING_MG: Psqt = [
    0, 0,  0,   0,   0, 0,  0, 0,
    0, 0,  0,   0,   0, 0,  0, 0,
    0, 0,  0,   0,   0, 0,  0, 0,
    0, 0,  0,  20,  20, 0,  0, 0,
    0, 0,  0,  20,  20, 0,  0, 0,
    0, 0,  0,   0,   0, 0,  0, 0,
    0, 0,  0, -10, -10, 0,  0, 0,
    0, 0, 20, -10, -10, 0, 20, 0,
];

#[rustfmt::skip]
const QUEEN_MG: Psqt = [
    870, 880, 890, 890, 890, 890, 880, 870,
    880, 890, 895, 895, 895, 895, 890, 880,
    890, 895, 910, 910, 910, 910, 895, 890,
    890, 895, 910, 920, 920, 910, 895, 890,
    890, 895, 910, 920, 920, 910, 895, 890,
    890, 895, 895, 895, 895, 895, 895, 890,
    880, 890, 895, 895, 895, 895, 890, 880,
    870, 880, 890, 890, 890, 890, 880, 870 
];

#[rustfmt::skip]
const ROOK_MG: Psqt = [
   500, 500, 500, 500, 500, 500, 500, 500,
   515, 515, 515, 520, 520, 515, 515, 515,
   500, 500, 500, 500, 500, 500, 500, 500,
   500, 500, 500, 500, 500, 500, 500, 500,
   500, 500, 500, 500, 500, 500, 500, 500,
   500, 500, 500, 500, 500, 500, 500, 500,
   500, 500, 500, 500, 500, 500, 500, 500,
   500, 500, 500, 510, 510, 510, 500, 500
];

#[rustfmt::skip]
const BISHOP_MG: Psqt = [
    300, 320, 320, 320, 320, 320, 320, 300,
    305, 320, 320, 320, 320, 320, 320, 305,
    310, 320, 320, 325, 325, 320, 320, 310,
    310, 330, 330, 350, 350, 330, 330, 310,
    325, 325, 330, 345, 345, 330, 325, 325,
    325, 325, 325, 330, 330, 325, 325, 325,
    310, 325, 325, 330, 330, 325, 325, 310,
    300, 310, 310, 310, 310, 310, 310, 300
];

#[rustfmt::skip]
const KNIGHT_MG: Psqt = [
    290, 300, 300, 300, 300, 300, 300, 290,
    300, 305, 305, 305, 305, 305, 305, 300,
    300, 305, 325, 325, 325, 325, 305, 300,
    300, 305, 325, 325, 325, 325, 305, 300,
    300, 305, 325, 325, 325, 325, 305, 300,
    300, 305, 320, 325, 325, 325, 305, 300,
    300, 305, 305, 305, 305, 305, 305, 300,
    290, 310, 300, 300, 300, 300, 310, 290
];

#[rustfmt::skip]
const PAWN_MG: Psqt = [
    100, 100, 100, 100, 100, 100, 100, 100,
    160, 160, 160, 160, 170, 160, 160, 160,
    140, 140, 140, 150, 160, 140, 140, 140,
    120, 120, 120, 140, 150, 120, 120, 120,
    105, 105, 115, 130, 140, 110, 105, 105,
    105, 105, 110, 120, 130, 105, 105, 105,
    105, 105, 105,  70,  70, 105, 105, 105,
    100, 100, 100, 100, 100, 100, 100, 100
];

pub const PSQT_MG: [Psqt; Piece::_RESERVED_PIECE_COUNT.to_index()] =
    [KING_MG, QUEEN_MG, ROOK_MG, BISHOP_MG, KNIGHT_MG, PAWN_MG];

#[rustfmt::skip]
pub const FLIP: [usize; 64] = [
    56, 57, 58, 59, 60, 61, 62, 63,
    48, 49, 50, 51, 52, 53, 54, 55,
    40, 41, 42, 43, 44, 45, 46, 47,
    32, 33, 34, 35, 36, 37, 38, 39,
    24, 25, 26, 27, 28, 29, 30, 31,
    16, 17, 18, 19, 20, 21, 22, 23,
     8,  9, 10, 11, 12, 13, 14, 15,
     0,  1,  2,  3,  4,  5,  6,  7,
];
