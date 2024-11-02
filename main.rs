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

const ROOK_MAGICS: [u64; 64] = [
    0x8a80104000800020u64,
    0x140002000100040u64,
    0x2801880a0017001u64,
    0x100081001000420u64,
    0x200020010080420u64,
    0x3001c0002010008u64,
    0x8480008002000100u64,
    0x2080088004402900u64,
    0x800098204000u64,
    0x2024401000200040u64,
    0x100802000801000u64,
    0x120800800801000u64,
    0x208808088000400u64,
    0x2802200800400u64,
    0x2200800100020080u64,
    0x801000060821100u64,
    0x80044006422000u64,
    0x100808020004000u64,
    0x12108a0010204200u64,
    0x140848010000802u64,
    0x481828014002800u64,
    0x8094004002004100u64,
    0x4010040010010802u64,
    0x20008806104u64,
    0x100400080208000u64,
    0x2040002120081000u64,
    0x21200680100081u64,
    0x20100080080080u64,
    0x2000a00200410u64,
    0x20080800400u64,
    0x80088400100102u64,
    0x80004600042881u64,
    0x4040008040800020u64,
    0x440003000200801u64,
    0x4200011004500u64,
    0x188020010100100u64,
    0x14800401802800u64,
    0x2080040080800200u64,
    0x124080204001001u64,
    0x200046502000484u64,
    0x480400080088020u64,
    0x1000422010034000u64,
    0x30200100110040u64,
    0x100021010009u64,
    0x2002080100110004u64,
    0x202008004008002u64,
    0x20020004010100u64,
    0x2048440040820001u64,
    0x101002200408200u64,
    0x40802000401080u64,
    0x4008142004410100u64,
    0x2060820c0120200u64,
    0x1001004080100u64,
    0x20c020080040080u64,
    0x2935610830022400u64,
    0x44440041009200u64,
    0x280001040802101u64,
    0x2100190040002085u64,
    0x80c0084100102001u64,
    0x4024081001000421u64,
    0x20030a0244872u64,
    0x12001008414402u64,
    0x2006104900a0804u64,
    0x1004081002402u64,
];

const BISHOP_MAGICS: [u64; 64] = [
    0x40040844404084u64,
    0x2004208a004208u64,
    0x10190041080202u64,
    0x108060845042010u64,
    0x581104180800210u64,
    0x2112080446200010u64,
    0x1080820820060210u64,
    0x3c0808410220200u64,
    0x4050404440404u64,
    0x21001420088u64,
    0x24d0080801082102u64,
    0x1020a0a020400u64,
    0x40308200402u64,
    0x4011002100800u64,
    0x401484104104005u64,
    0x801010402020200u64,
    0x400210c3880100u64,
    0x404022024108200u64,
    0x810018200204102u64,
    0x4002801a02003u64,
    0x85040820080400u64,
    0x810102c808880400u64,
    0xe900410884800u64,
    0x8002020480840102u64,
    0x220200865090201u64,
    0x2010100a02021202u64,
    0x152048408022401u64,
    0x20080002081110u64,
    0x4001001021004000u64,
    0x800040400a011002u64,
    0xe4004081011002u64,
    0x1c004001012080u64,
    0x8004200962a00220u64,
    0x8422100208500202u64,
    0x2000402200300c08u64,
    0x8646020080080080u64,
    0x80020a0200100808u64,
    0x2010004880111000u64,
    0x623000a080011400u64,
    0x42008c0340209202u64,
    0x209188240001000u64,
    0x400408a884001800u64,
    0x110400a6080400u64,
    0x1840060a44020800u64,
    0x90080104000041u64,
    0x201011000808101u64,
    0x1a2208080504f080u64,
    0x8012020600211212u64,
    0x500861011240000u64,
    0x180806108200800u64,
    0x4000020e01040044u64,
    0x300000261044000au64,
    0x802241102020002u64,
    0x20906061210001u64,
    0x5a84841004010310u64,
    0x4010801011c04u64,
    0xa010109502200u64,
    0x4a02012000u64,
    0x500201010098b028u64,
    0x8040002811040900u64,
    0x28000010020204u64,
    0x6000020202d0240u64,
    0x8918844842082200u64,
    0x4010011029020020u64,
];

const ROOK_RELLEVANT_BITS: [u8; 64] = [
    12, 11, 11, 11, 11, 11, 11, 12,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    12, 11, 11, 11, 11, 11, 11, 12
];

const BISHOP_RELLEVANT_BITS: [u8; 64] = [
    6, 5, 5, 5, 5, 5, 5, 6,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 5, 5, 5, 5, 5, 5, 6
];

fn mask_rook_attacks(square: u64) -> u64
{
    // attacks bitboard
    let mut attacks = 0u64;
    
    // init files & ranks
    let mut f = 0;
    let mut r = 0;
    
    // init target files & ranks
    let tr = square / 8;
    let tf = square % 8;
    
    // generate attacks
    for r in (tr + 1)..=6 {
        attacks |= 1 << (r * 8 + tf);
    }

    // Move downwards from the current row (tr), keeping the file (tf) constant
    for r in (1..tr).rev() {
        attacks |= 1 << (r * 8 + tf);
    }

    // Move rightwards from the current file (tf), keeping the row (tr) constant
    for f in (tf + 1)..=6 {
        attacks |= 1 << (tr * 8 + f);
    }

    // Move leftwards from the current file (tf), keeping the row (tr) constant
    for f in (1..tf).rev() {
        attacks |= 1 << (tr * 8 + f);
    }
    
    // return attack map for bishop on a given square
    return attacks;
}

fn mask_bishop_attacks(square: u64) -> u64 {
    // attack bitboard
    let mut attacks = 0u64;
    
    // init target files & ranks
    let tr = square / 8;
    let tf = square % 8;
    
    // generate attacks
    for (mut r, mut f) in (tr + 1..=6).zip(tf + 1..=6) {
        attacks |= 1 << (r * 8 + f);
    }

    for (mut r, mut f) in (tr + 1..=6).zip((1..=tf).rev()) {
        attacks |= 1 << (r * 8 + f);
    }

    for (mut r, mut f) in (1..=tr).rev().zip(tf + 1..=6) {
        attacks |= 1 << (r * 8 + f);
    }

    for (mut r, mut f) in (1..=tr).rev().zip((1..=tf).rev()) {
        attacks |= 1 << (r * 8 + f);
    }
    
    // return attack map for bishop on a given square
    return attacks
}

fn count_bits(mut bitboard: u64) -> usize {
  // bit count
  let mut count = 0;
  
  // pop bits untill bitboard is empty
  while (bitboard != 0)
  {
      // increment count
      count += 1;
      
      // consecutively reset least significant 1st bit
      bitboard &= bitboard - 1;
  }
  
  // return bit count
  return count;
}

fn get_ls1b_index(bitboard: u64) -> usize {
    if (bitboard != 0) {
        count_bits(((bitboard as i64 & -(bitboard as i64)) - 1) as _)
    } else {
        0
    }
}

fn pop_bit(bitboard: &mut u64, square: u64) -> u64 {
    if (*bitboard >> square) & 1 == 1 {
        *bitboard ^= 1 << square;
        1
    } else {
        0
    }
}

fn set_occupancy(index: usize, bits_in_mask: usize, mut attack_mask: u64) -> u64
{
    // occupancy map
    let mut occupancy = 0u64;
    
    for count in 0..bits_in_mask {
        // get LS1B index of attacks mask
        let square = get_ls1b_index(attack_mask);
        
        // pop LS1B in attack map
        pop_bit(&mut attack_mask, square as _);
        
        // make sure occupancy is on board
        if ((index & (1 << count)) != 0) {
            // populate occupancy map
            occupancy |= (1u64 << square);
        }
    }
    
    // return occupancy map
    return occupancy;
}

fn bishop_attacks_on_the_fly(square: u64, block: u64) -> u64 {
    let mut attacks = 0u64;

    let tr = square / 8;
    let tf = square % 8;

    // Generate attacks in the four diagonal directions
    for (mut r, mut f) in (tr + 1..=7).zip((tf + 1)..=7) {
        attacks |= 1 << (r * 8 + f);
        if block & (1 << (r * 8 + f)) != 0 {
            break;
        }
    }

    for (mut r, mut f) in (tr + 1..=7).zip((0..tf).rev()) {
        attacks |= 1 << (r * 8 + f);
        if block & (1 << (r * 8 + f)) != 0 {
            break;
        }
    }

    for (mut r, mut f) in (0..tr).rev().zip((tf + 1)..=7) {
        attacks |= 1 << (r * 8 + f);
        if block & (1 << (r * 8 + f)) != 0 {
            break;
        }
    }

    for (mut r, mut f) in (0..tr).rev().zip((0..tf).rev()) {
        attacks |= 1 << (r * 8 + f);
        if block & (1 << (r * 8 + f)) != 0 {
            break;
        }
    }

    attacks
}

fn rook_attacks_on_the_fly(square: u64, block: u64) -> u64 {
    let mut attacks = 0u64;

    let tr = square / 8;
    let tf = square % 8;

    // Generate attacks in the four orthogonal directions
    for r in (tr + 1)..=7 {
        attacks |= 1 << (r * 8 + tf);
        if block & (1 << (r * 8 + tf)) != 0 {
            break;
        }
    }

    for r in (0..tr).rev() {
        attacks |= 1 << (r * 8 + tf);
        if block & (1 << (r * 8 + tf)) != 0 {
            break;
        }
    }

    for f in (tf + 1)..=7 {
        attacks |= 1 << (tr * 8 + f);
        if block & (1 << (tr * 8 + f)) != 0 {
            break;
        }
    }

    for f in (0..tf).rev() {
        attacks |= 1 << (tr * 8 + f);
        if block & (1 << (tr * 8 + f)) != 0 {
            break;
        }
    }

    attacks
}

fn init_sliders_attacks(bishop_masks: &mut [u64; 64], rook_masks: &mut [u64; 64], bishop_attacks: &mut [[u64; 512]; 64], rook_attacks: &mut [[u64; 4096]; 64], is_bishop: bool)
{
    for square in 0..64 {
        // init bishop & rook masks
        bishop_masks[square] = mask_bishop_attacks(square as _);
        rook_masks[square] = mask_rook_attacks(square as _);
        
        // init current mask
        let mask = if is_bishop { mask_bishop_attacks(square as _) } else { mask_rook_attacks(square as _) };
        
        // count attack mask bits
        let bit_count = count_bits(mask);
        
        // occupancy variations count
        let occupancy_variations = 1 << bit_count;
        
        // loop over occupancy variations
        for count in 0..occupancy_variations {
            // bishop
            if (is_bishop)
            {
                // init occupancies, magic index & attacks
                let occupancy = set_occupancy(count, bit_count, mask);
                let magic_index = occupancy * BISHOP_MAGICS[square] >> 64 - BISHOP_RELLEVANT_BITS[square];
                bishop_attacks[square][magic_index as usize] = bishop_attacks_on_the_fly(square as _, occupancy);                
            
            }
            // rook
            else
            {
                // init occupancies, magic index & attacks
                let occupancy = set_occupancy(count, bit_count, mask);
                let magic_index = occupancy * ROOK_MAGICS[square] >> 64 - ROOK_RELLEVANT_BITS[square];
                rook_attacks[square][magic_index as usize] = rook_attacks_on_the_fly(square as _, occupancy);                
            }
        }
    }
}

fn get_bishop_attacks(bishop_masks: &[u64; 64], bishop_attacks: &[[u64; 512]; 64], square: u64, mut occupancy: u64) -> u64 {
    let square = square as usize;

	occupancy &= bishop_masks[square];
	occupancy *=  BISHOP_MAGICS[square];
	occupancy >>= 64 - BISHOP_RELLEVANT_BITS[square];

    let occupancy = occupancy as usize;
	
	bishop_attacks[square][occupancy]
}

fn get_rook_attacks(rook_masks: &[u64; 64], rook_attacks: &[[u64; 4096]; 64], square: u64, mut occupancy: u64) -> u64 {
    let square = square as usize;

	occupancy &= rook_masks[square];
	occupancy *=  ROOK_MAGICS[square];
	occupancy >>= 64 - ROOK_RELLEVANT_BITS[square];

    let occupancy = occupancy as usize;	

	rook_attacks[square][occupancy]
}

fn print_bitboard(bitboard: u64) {
    println!();
    
    for rank in 0..8 {
        for file in 0..8 {
            let square = rank * 8 + file;
            
            // Print ranks on the left
            if file == 0 {
                print!("  {} ", 8 - rank);
            }
            
            // Print the bit at the given square
            print!(" {}", if (bitboard >> square) & 1 == 1 { 1 } else { 0 });
        }
        
        println!();
    }
    
    // Print files labels
    println!("\n     a b c d e f g h\n");
    
    // Print bitboard as decimal
    println!("     bitboard: {}\n", bitboard);
}

fn main() {
    // fn print_knight_possible_moves(square: usize) {
    //     let board = KNIGHT_OFFSET_BOARDS[square];
    //     let knight_square = flatten_to_chess_pos(square);
    //     let moves = board.iter().map(flatten_to_chess_pos).collect::<Vec::<_>>();

    //     println!("knight square: {knight_square}");
    //     println!("possible knight moves from square {knight_square}: {moves:?}");
    // }

    // Board::knights().iter().for_each(print_knight_possible_moves);

    let mut bishop_masks = [0u64; 64];
    let mut rook_masks = [0u64; 64];

    let mut bishop_attacks = [[0u64; 512]; 64];
    let mut rook_attacks = [[0u64; 4096]; 64];

    init_sliders_attacks(&mut bishop_masks, &mut rook_masks, &mut bishop_attacks, &mut rook_attacks, false);
    init_sliders_attacks(&mut bishop_masks, &mut rook_masks, &mut bishop_attacks, &mut rook_attacks, true);

    print_bitboard(get_rook_attacks(&rook_masks, &rook_attacks, 28, 0));
}

/* TODO:
    Magic bit boards,
    Offsets for king and pawn
*/
