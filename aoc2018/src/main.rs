use std::fs::File;
use std::io::{BufRead, BufReader};

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;

fn main() {
    day1::solve(read_data(1));
    day2::solve(read_data(2));
    day3::solve(read_data(3));
    day4::solve(read_data(4));
    day5::solve(read_data(5));
    day6::solve(read_data(6));
    day7::solve(read_data(7));
    day8::solve(read_data(8));
    day9::solve(read_data(9));
    day10::solve(read_data(10));
    day11::solve(read_data(11));
    day12::solve(read_data(12));
    day13::solve(read_data(13));
    day14::solve(read_data(14));
    day15::solve(read_data(15));
    day16::solve(read_data(16));
    day17::solve(read_data(17));
    day18::solve(read_data(18));
    day19::solve(read_data(19));
    day20::solve(read_data(20));
    day21::solve(read_data(21));
    day22::solve(read_data(22));
    day23::solve(read_data(23));
    day24::solve(read_data(24));
    day25::solve(read_data(25));
}

fn read_data(day: u8) -> Vec<String> {
    match File::open(format!("data/{}.txt", day)) {
        Err(_) => { vec![] },
        Ok(file) => {
            let reader = BufReader::new(&file);
            reader.lines().map(|line| line.unwrap()).collect()
        }
    }
}
