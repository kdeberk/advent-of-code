
use std::fs::File;
use std::io::{BufRead, BufReader};

fn react(a: char, b: char) -> bool {
    a != b && a.to_ascii_lowercase() == b.to_ascii_lowercase()
}

fn reduce(chars: Vec<char>) -> Vec<char> {
    let mut reduced = vec![];

    for ch in chars {
        if 0 == reduced.len() || ! react(ch, reduced[reduced.len() - 1]) {
            reduced.push(ch);
        } else {
            reduced.pop();
        }
    }

    reduced
}

fn remove_all_of(chars: &Vec<char>, to_remove: char) -> Vec<char> {
    let mut result = chars.clone();
    result.retain(|c| c.to_ascii_lowercase() != to_remove);
    result
}

fn part_1(chars: &Vec<char>) {
    println!("{}", reduce(chars.clone()).len());
}

fn part_2(chars: &Vec<char>) {
    let mut best_length = chars.len();

    for c in "abcdefghijklmnopqrstuvwxyz".chars() {
        let length = reduce(remove_all_of(&chars, c)).len();
        println!("{}: {}", c, length);

        if best_length > length {
            best_length = length;
        }
    }

    println!("{}", best_length)
}

fn main() {
    let file = File::open("input").unwrap();
    let mut reader = BufReader::new(&file);
    let mut string = String::new();
    reader.read_line(&mut string).unwrap();

    let chars:Vec<char> = string.chars().collect();

    part_1(&chars);
    part_2(&chars);
}
