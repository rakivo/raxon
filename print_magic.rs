// NOTE: some of masks and functions here is translated `C` code from here: https://github.com/maksimKorzh/chess_programming/blob/master/src/magics/magics.c, so I'm going to paste the license thingy to show respect.

/**************************************\
 ======================================
        
         Plain magic bitboards
     implementation & demonstration
     
            translated from
                  
           Code Monkey King

 ======================================
\**************************************/

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

pub const BISHOP_MAGICS: [u64; 64] = [
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

const ROOK_RELEVANT_BITS: [u8; 64] = [
    12, 11, 11, 11, 11, 11, 11, 12,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    12, 11, 11, 11, 11, 11, 11, 12
];

const BISHOP_RELEVANT_BITS: [u8; 64] = [
    6, 5, 5, 5, 5, 5, 5, 6,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 5, 5, 5, 5, 5, 5, 6
];

const fn mask_rook_attacks(square: u64) -> u64 {
    let mut attacks = 0u64;
    
    let tr = square / 8;
    let tf = square % 8;
    
    if tr < 7 {
        let mut r = tr + 1;
        while r <= 6 {
            attacks |= 1 << (r * 8 + tf);
            r += 1;
        }
    }

    if tr > 0 {
        let mut r = tr - 1;
        while r >= 1 {
            attacks |= 1 << (r * 8 + tf);
            r -= 1;
        }
    }

    if tf < 7 {
        let mut f = tf + 1;
        while f <= 6 {
            attacks |= 1 << (tr * 8 + f);
            f += 1;
        }
    }
    
    if tf > 0 {
        let mut f = tf - 1;
        while f >= 1 {
            attacks |= 1 << (tr * 8 + f);
            f -= 1;
        }
    }
    
    attacks
}

const fn mask_bishop_attacks(square: u64) -> u64 {
    let mut attacks = 0u64;
    
    let tr = square / 8;
    let tf = square % 8;
    
    if tf < 7 && tr < 7 {
        let mut r = tr + 1;
        let mut f = tf + 1;
        while r <= 6 && f <= 6 {
            attacks |= 1 << (r * 8 + f);
            r += 1;
            f += 1;
        }
    }
    
    if tf > 0 && tr < 7 {
        let mut r = tr + 1;
        let mut f = tf - 1;
        while r <= 6 && f >= 1 {
            attacks |= 1 << (r * 8 + f);
            r += 1;
            f -= 1;
        }
    }
    
    if tf < 7 && tr > 0 {
        let mut r = tr - 1;
        let mut f = tf + 1;
        while r >= 1 && f <= 6 {
            attacks |= 1 << (r * 8 + f);
            r -= 1;
            f += 1;
        }
    }
    
    if tf > 0 && tr > 0 {
        let mut r = tr - 1;
        let mut f = tf - 1;
        while r >= 1 && f >= 1 {
            attacks |= 1 << (r * 8 + f);
            r -= 1;
            f -= 1;
        }
    }
    
    attacks
}

const fn count_bits(mut bitboard: u64) -> usize {
    let mut count = 0;
    while bitboard != 0 {
        count += 1;
        bitboard &= bitboard - 1;
    }
    count
}

const fn get_ls1b_index(mut bitboard: u64) -> usize {
    let mut count = 0;
    while bitboard > 0 && (bitboard & 1) == 0 {
        bitboard >>= 1;
        count += 1;
    } count
}

const fn pop_bit(bitboard: &mut u64, square: u64) -> u64 {
    let is_set = (*bitboard >> square) & 1 == 1;
    if is_set {
        *bitboard ^= 1 << square;
        1
    } else {
        0
    }
}

const fn set_occupancy(index: usize, bits_in_mask: usize, mut attack_mask: u64) -> u64 {
    let mut occupancy = 0;
    let mut count = 0;
    while count < bits_in_mask {
        let square = get_ls1b_index(attack_mask) as u64;
        pop_bit(&mut attack_mask, square);

        if (index & (1 << count)) != 0 {
            occupancy |= 1 << square;
        }
        count += 1;
    }
    occupancy
}

const fn init_bishop_attacks(bishop_masks: &mut [u64; 64], bishop_attacks: &mut [[u64; 512]; 64]) {
    let mut square = 0;
    while square < 64 {
        bishop_masks[square] = mask_bishop_attacks(square as u64);
        let mask = bishop_masks[square];
        let bit_count = count_bits(mask);
        let occupancy_variations = 1 << bit_count;
        
        let mut count = 0;
        while count < occupancy_variations {
            let occupancy = set_occupancy(count, bit_count, mask);
            let magic_index = (occupancy * BISHOP_MAGICS[square] >> 
                             (64 - BISHOP_RELEVANT_BITS[square])) as usize;
            
            bishop_attacks[square][magic_index] = 
                bishop_attacks_on_the_fly(square as u64, occupancy);
            
            count += 1;
        }
        square += 1;
    }
}

const fn init_rook_attacks(rook_masks: &mut [u64; 64], rook_attacks: &mut [[u64; 4096]; 64]) {
    let mut square = 0;
    while square < 64 {
        rook_masks[square] = mask_rook_attacks(square as u64);
        let mask = rook_masks[square];
        let bit_count = count_bits(mask);
        let occupancy_variations = 1 << bit_count;
        
        let mut count = 0;
        while count < occupancy_variations {
            let occupancy = set_occupancy(count, bit_count, mask);
            let magic_index = (occupancy * ROOK_MAGICS[square] >> 
                             (64 - ROOK_RELEVANT_BITS[square])) as usize;
            
            rook_attacks[square][magic_index] = rook_attacks_on_the_fly(square as u64, occupancy);
            
            count += 1;
        }
        square += 1;
    }
}

const fn bishop_attacks_on_the_fly(square: u64, block: u64) -> u64 {
    let mut attacks = 0u64;
    let tr = square / 8;
    let tf = square % 8;
    
    if tf < 7 && tr < 7 {
        let mut r = tr + 1;
        let mut f = tf + 1;
        while r < 8 && f < 8 {
            attacks |= 1 << (r * 8 + f);
            if block & (1 << (r * 8 + f)) != 0 {
                break;
            }
            r += 1;
            f += 1;
        }
    }
    
    if tf > 0 && tr < 7 {
        let mut r = tr + 1;
        let mut f = tf - 1;
        while r < 8 && f < 8 {
            attacks |= 1 << (r * 8 + f);
            if block & (1 << (r * 8 + f)) != 0 {
                break;
            }
            r += 1;
            f -= 1;
        }
    }
    
    if tf < 7 && tr > 0 {
        let mut r = tr - 1;
        let mut f = tf + 1;
        while r < 8 && f < 8 {
            attacks |= 1 << (r * 8 + f);
            if block & (1 << (r * 8 + f)) != 0 {
                break;
            }
            r -= 1;
            f += 1;
        }
    }
    
    if tf > 0 && tr > 0 {
        let mut r = tr - 1;
        let mut f = tf - 1;
        while r < 8 && f < 8 {
            attacks |= 1 << (r * 8 + f);
            if block & (1 << (r * 8 + f)) != 0 {
                break;
            }
            r -= 1;
            f -= 1;
        }
    }
    
    attacks
}

const fn rook_attacks_on_the_fly(square: u64, block: u64) -> u64 {
    let mut attacks = 0u64;
    let tr = square / 8;
    let tf = square % 8;
    
    if tr < 7 {
        let mut r = tr + 1;
        while r < 8 {
            attacks |= 1 << (r * 8 + tf);
            if block & (1 << (r * 8 + tf)) != 0 {
                break;
            }
            r += 1;
        }
    }
    
    if tr > 0 {
        let mut r = tr - 1;
        while r < 8 {
            attacks |= 1 << (r * 8 + tf);
            if block & (1 << (r * 8 + tf)) != 0 {
                break;
            }
            r -= 1;
        }
    }
    
    if tf < 7 {
        let mut f = tf + 1;
        while f < 8 {
            attacks |= 1 << (tr * 8 + f);
            if block & (1 << (tr * 8 + f)) != 0 {
                break;
            }
            f += 1;
        }
    }
    
    if tf > 0 {
        let mut f = tf - 1;
        while f < 8 {
            attacks |= 1 << (tr * 8 + f);
            if block & (1 << (tr * 8 + f)) != 0 {
                break;
            }
            f -= 1;
        }
    }
    
    attacks
}

#[allow(unused)]
fn print_bitboard(bitboard: u64) {
    for rank in 0..8 {
        for file in 0..8 {
            let square = rank * 8 + file;
            if file == 0 {
                print!("  {rank} ", rank = 8 - rank);
            } 
            print!(" {bit}", bit = if (bitboard >> square) & 1 == 1 { 1 } else { 0 });
        } println!();
    }
    
    println!("\n     a b c d e f g h");
    println!("     bitboard: {bitboard:064b}")
}

fn get_pawn_moves() -> ([u64; 64], [u64; 64], [u64; 64], [u64; 64]) {
    let mut single_push_white   = [0u64; 64];
    let mut single_push_black   = [0u64; 64];
    let mut double_push_white   = [0u64; 64];
    let mut double_push_black   = [0u64; 64];
    let mut capture_left_white  = [0u64; 64];
    let mut capture_right_white = [0u64; 64];
    let mut capture_left_black  = [0u64; 64];
    let mut capture_right_black = [0u64; 64];

    #[inline(always)]
    const fn flip_square_vertically(square: usize) -> usize {
        (8 - 1 - square / 8) * 8 + square % 8
    }

    for _square in 0..64 {
        let row = _square / 8;
        let col = _square % 8;
        let square = flip_square_vertically(_square);
        let bit = 1u64 << _square;

        if square < 56 {
            single_push_black[square] = bit << 8;
        }

        if square >= 8 {
            single_push_white[square] = bit >> 8;
        }

        if row == 6 {
            double_push_white[square] = bit >> 16;
        }

        if row == 1 {
            double_push_black[square] = bit << 16;
        }

        if col != 0 && square < 56 {
            capture_left_black[square] = bit << 7;
        }

        if col != 7 && square < 56 {
            capture_right_black[square] = bit << 9;
        }

        if col != 0 && square >= 8 {
            capture_right_white[square] = bit >> 9;
        }

        if col != 7 && square >= 8 {
            capture_left_white[square] = bit >> 7;
        }
    }

    let mut white_pawn_moves = [0u64; 64];
    let mut black_pawn_moves = [0u64; 64];
    let mut white_pawn_attacks = [0u64; 64];
    let mut black_pawn_attacks = [0u64; 64];

    for i in 0..64 {
        white_pawn_moves[i] = single_push_white[i] | double_push_white[i];
        black_pawn_moves[i] = single_push_black[i] | double_push_black[i];
        white_pawn_attacks[i] = capture_left_white[i] | capture_right_white[i];
        black_pawn_attacks[i] = capture_left_black[i] | capture_right_black[i];
    }

    (white_pawn_moves, black_pawn_moves, white_pawn_attacks, black_pawn_attacks)
}

fn print_relevant_bits() {
    print!("pub const ROOK_RELEVANT_BITS: [u8; 64] = [");
    for value in ROOK_RELEVANT_BITS.iter() {
        print!("{value:#018x},");
    }
    println!("];");
    println!("pub const BISHOP_RELEVANT_BITS: [u8; 64] = [");
    for value in BISHOP_RELEVANT_BITS.iter() {
        print!("{value:#018x},");
    }
    println!("];");
}

fn print_constants() {
    let mut rook_masks = [0u64; 64];
    let mut rook_attacks = [[0u64; 4096]; 64];
    init_rook_attacks(&mut rook_masks, &mut rook_attacks);

    print!("pub const ROOK_MASKS: [u64; 64] = [");
    for value in rook_masks.iter() {
        print!("{value:#018x},");
    }
    println!("];\n");
    print!("pub const ROOK_ATTACKS: [[u64; 4096]; 64] = [");
    for row in rook_attacks.iter() {
        print!("[");
        for value in row.iter() {
            print!("{value:#018x}, ");
        }
        print!("],");
    }
    println!("];");

    let mut bishop_masks = [0u64; 64];
    let mut bishop_attacks = [[0u64; 512]; 64];
    init_bishop_attacks(&mut bishop_masks, &mut bishop_attacks);

    print!("pub const BISHOP_MASKS: [u64; 64] = [");
    for value in bishop_masks.iter() {
        print!("{value:#018x},")
    }
    println!("];\n");
    print!("pub const BISHOP_ATTACKS: [[u64; 512]; 64] = [");
    for row in bishop_attacks.iter() {
        print!("[");
        for value in row.iter() {
            print!("{value:#018x}, ");
        }
        print!("],");
    }
    println!("];");

    let (white_pawn_moves, black_pawn_moves, white_pawn_attacks, black_pawn_attacks) = get_pawn_moves();

    print!("pub const WHITE_PAWN_MOVES: [u64; 64] = [");
    for value in white_pawn_moves.iter() {
        print!("{value:#018x},");
    }
    println!("];");

    print!("pub const WHITE_PAWN_ATTACKS: [u64; 64] = [");
    for value in white_pawn_attacks.iter() {
        print!("{value:#018x},");
    }
    println!("];");

    print!("pub const BLACK_PAWN_MOVES: [u64; 64] = [");
    for value in black_pawn_moves.iter() {
        print!("{value:#018x},");
    }
    println!("];");

    print!("pub const BLACK_PAWN_ATTACKS: [u64; 64] = [");
    for value in black_pawn_attacks.iter() {
        print!("{value:#018x},");
    }
    println!("];");
}

fn print_magics() {
    print!("pub const ROOK_MAGICS: [u64; 64] = [");
    for value in ROOK_MAGICS.iter() {
        print!("{value:#018x},");
    }
    println!("];");
    print!("pub const BISHOP_MAGICS: [u64; 64] = [");
    for value in BISHOP_MAGICS.iter() {
        print!("{value:#018x},");
    }
    println!("];");
}

fn main() {
    print_magics();
    print_constants();
    print_relevant_bits();
}
