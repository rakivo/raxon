use std::io::BufRead;
use std::fs::{File, OpenOptions};

use crate::{Move, Board};

pub struct UCI {
    pub engine: Board,
    debug_output: File
}

impl UCI {
    pub fn new() -> std::io::Result::<Self> {
        Ok(Self {
            engine: Board::new(),
            debug_output: OpenOptions::new()
                .create(true)
                .append(true)
                .open("./engine_debug.log")?
        })
    }

    // Start the UCI interface loop
    pub fn start(&mut self) {
        println!("id name MyChessEngine v1.0");
        println!("id author YourName");
        println!("uciok");

        for line in std::io::stdin().lock().lines() {
            let line = line.unwrap();
            let args = line.trim().split_whitespace().collect::<Vec::<_>>();
            match args.get(0) {
                Some(&"uci") => self.handle_uci(),
                Some(&"isready") => self.handle_isready(),
                Some(&"setoption") => self.handle_setoption(&args),
                Some(&"position") => self.handle_position(&args),
                Some(&"go") => self.handle_go(&args),
                Some(&"quit") => {
                    self.handle_quit();
                    break
                }
                _ => println!("unknown command"),
            }
        }
    }

    fn handle_uci(&self) {
        println!("id name MyChessEngine");
        println!("id author YourName");
        println!("uciok");
    }

    fn handle_isready(&self) {
        println!("readyok");
    }

    fn handle_setoption(&self, args: &[&str]) {
        if args.len() < 3 { return }
        let option = args[1];
        let value = args[2];
        println!("setoption {option} {value}");
    }

    pub fn handle_position(&mut self, args: &[&str]) {
        if args.len() < 2 {
            // Invalid command (at least "position" and some argument are needed)
            return;
        }

        // If the second argument is "startpos", use the starting position.
        if args[1] == "startpos" {
            self.engine.reset_positions();
            if args.len() > 2 && args[2] == "moves" {
                let moves = &args[3..];
                println!("Position set to startpos with moves: {:?}", moves);
                for mv in moves {
                    let mv = Move::from_algebraic_notation(mv);
                    self.engine.make_move(mv);
                }
            }
            println!("Position set to startpos");
        }
        // If the second argument is "fen", parse the FEN string
        else if args[1] == "fen" {
            if args.len() < 3 { return }
            let fen = args[2];
            self.engine.fen_set(fen);
            println!("Position set to FEN: {fen}")
        }
    }

    pub fn handle_go(&mut self, args: &[&str]) {
        // Check if there are any parameters passed with "go" (like time, depth, etc.)
        #[allow(unused)]
        let mut depth = None;
        #[allow(unused)]
        let mut time = None;

        // Parse the arguments after "go"
        for i in 1..args.len() {
            match args[i] {
                // If the argument is "depth", set the search depth
                "depth" => {
                    if let Some(d) = args.get(i + 1) {
                        depth = Some(d.parse::<u32>().unwrap_or(0));
                    }
                }
                // If the argument is "time", set the thinking time (in milliseconds)
                "time" => {
                    if let Some(t) = args.get(i + 1) {
                        time = Some(t.parse::<u64>().unwrap_or(0));
                    }
                }
                // If the argument is "movetime", set the time to think for one move (in milliseconds)
                "movetime" => {
                    if let Some(t) = args.get(i + 1) {
                        time = Some(t.parse::<u64>().unwrap_or(0));
                    }
                }
                _ => {}
            }
        }

        let legal_moves = self.engine.legal_moves();
        println!("legal moves:");
        legal_moves.iter().for_each(|mv| println!("src: {src}, dst: {dst}", src = mv.src_square(), dst = mv.dst_square()));
        let best_move = legal_moves[0].to_algebraic_notation();
        // writeln!(self.debug_output, "legal moves: {legal_moves:?}").unwrap();
        println!("bestmove {best_move}");
    }

    fn handle_quit(&self) {
        println!("Goodbye!")
    }
}
