#[macro_export]
macro_rules! static_assert {
    ($constexpr: expr, $($msg: tt) *) => { const _: () = assert!($constexpr, $($msg) *); }
}

#[macro_export]
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

#[macro_export]
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

#[macro_export]
macro_rules! for_all_pieces {
    ($fn_name: ident -> $($ret: tt) *) => { paste::paste! {
        #[inline(always)]
        pub const fn white_pawns(&self) -> $($ret) * {
            self.$fn_name(Pieces::WhitePawns)
        }

        #[inline(always)]
        pub const fn black_pawns(&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackPawns)
        }

        #[inline(always)]
        pub const fn white_knights(&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKnights)
        }

        #[inline(always)]
        pub const fn black_knights(&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKnights)
        }

        #[inline(always)]
        pub const fn white_bishops(&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteBishops)
        }

        #[inline(always)]
        pub const fn black_bishops(&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackBishops)
        }

        #[inline(always)]
        pub const fn white_rooks(&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteRooks)
        }

        #[inline(always)]
        pub const fn black_rooks(&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackRooks)
        }

        #[inline(always)]
        pub const fn white_queens(&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteQueens)
        }

        #[inline(always)]
        pub const fn black_queens(&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackQueens)
        }

        #[inline(always)]
        pub const fn white_kings(&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKings)
        }

        #[inline(always)]
        pub const fn black_kings(&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKings)
        }

        #[inline(always)]
        pub const fn all_whites(&self) -> $($ret) * {
            self.$fn_name(Pieces::AllWhites)
        }

        #[inline(always)]
        pub const fn all_blacks(&self) -> $($ret) * {
            self.$fn_name(Pieces::AllBlacks)
        }
    }};

    (mut $fn_name: ident -> $($ret: tt) *) => { paste::paste! {
        #[inline(always)]
        pub const fn white_pawns(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhitePawns)
        }

        #[inline(always)]
        pub const fn black_pawns(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackPawns)
        }

        #[inline(always)]
        pub const fn white_knights(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKnights)
        }

        #[inline(always)]
        pub const fn black_knights(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKnights)
        }

        #[inline(always)]
        pub const fn white_bishops(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteBishops)
        }

        #[inline(always)]
        pub const fn black_bishops(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackBishops)
        }

        #[inline(always)]
        pub const fn white_rooks(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteRooks)
        }

        #[inline(always)]
        pub const fn black_rooks(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackRooks)
        }

        #[inline(always)]
        pub const fn white_queens(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteQueens)
        }

        #[inline(always)]
        pub const fn black_queens(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackQueens)
        }

        #[inline(always)]
        pub const fn white_kings(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKings)
        }

        #[inline(always)]
        pub const fn black_kings(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKings)
        }

        #[inline(always)]
        pub const fn all_whites(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::AllWhites)
        }

        #[inline(always)]
        pub const fn all_blacks(&mut self) -> $($ret) * {
            self.$fn_name(Pieces::AllBlacks)
        }
    }};

    ($postfix: ident $fn_name: ident -> $($ret: tt) *) => { paste::paste! {
        #[inline(always)]
        pub const fn [<white_pawns $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::WhitePawns)
        }

        #[inline(always)]
        pub const fn [<black_pawns $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackPawns)
        }

        #[inline(always)]
        pub const fn [<white_knights $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKnights)
        }

        #[inline(always)]
        pub const fn [<black_knights $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKnights)
        }

        #[inline(always)]
        pub const fn [<white_bishops $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteBishops)
        }

        #[inline(always)]
        pub const fn [<black_bishops $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackBishops)
        }

        #[inline(always)]
        pub const fn [<white_rooks $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteRooks)
        }

        #[inline(always)]
        pub const fn [<black_rooks $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackRooks)
        }

        #[inline(always)]
        pub const fn [<white_queens $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteQueens)
        }

        #[inline(always)]
        pub const fn [<black_queens $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackQueens)
        }

        #[inline(always)]
        pub const fn [<white_kings $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKings)
        }

        #[inline(always)]
        pub const fn [<black_kings $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKings)
        }

        #[inline(always)]
        pub const fn [<all_whites $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::AllWhites)
        }

        #[inline(always)]
        pub const fn [<all_blacks $postfix>](&self) -> $($ret) * {
            self.$fn_name(Pieces::AllBlacks)
        }
    }};

    (mut $postfix: ident $fn_name: ident -> $($ret: tt) *) => { paste::paste! {
        #[inline(always)]
        pub const fn [<white_pawns $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhitePawns)
        }

        #[inline(always)]
        pub const fn [<black_pawns $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackPawns)
        }

        #[inline(always)]
        pub const fn [<white_knights $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKnights)
        }

        #[inline(always)]
        pub const fn [<black_knights $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKnights)
        }

        #[inline(always)]
        pub const fn [<white_bishops $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteBishops)
        }

        #[inline(always)]
        pub const fn [<black_bishops $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackBishops)
        }

        #[inline(always)]
        pub const fn [<white_rooks $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteRooks)
        }

        #[inline(always)]
        pub const fn [<black_rooks $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackRooks)
        }

        #[inline(always)]
        pub const fn [<white_queens $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteQueens)
        }

        #[inline(always)]
        pub const fn [<black_queens $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackQueens)
        }

        #[inline(always)]
        pub const fn [<white_kings $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::WhiteKings)
        }

        #[inline(always)]
        pub const fn [<black_kings $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::BlackKings)
        }

        #[inline(always)]
        pub const fn [<all_whites $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::AllWhites)
        }

        #[inline(always)]
        pub const fn [<all_blacks $postfix>](&mut self) -> $($ret) * {
            self.$fn_name(Pieces::AllBlacks)
        }
    }};
}
