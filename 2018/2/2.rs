
use std::fs::File;
use std::io::{BufRead, BufReader};

fn distance(str_a: &str, str_b: &str) -> usize {
    let mut distance = 0;

    for (char_a, char_b) in str_a.chars().zip(str_b.chars()) {
        if char_a != char_b {
            distance += 1;
        }
    }

    distance
}

fn common(str_a: &str, str_b: &str) -> String {
    let mut string = String::new();

    for (char_a, char_b) in str_a.chars().zip(str_b.chars()) {
        if char_a == char_b {
            string.push(char_a)
        }
    }
    
    string
}

fn part_1(lines: &Vec<String>) {
    let mut twice_counts = 0;
    let mut thrice_counts = 0;

    for line in lines {
        let mut char_counts = [0u8; 26];

        for c in line.chars() {
            let index = ((c as u8) - ('a' as u8)) as usize;
            char_counts[index] += 1;
        }

        for i in 0..26 {
            if 2 == char_counts[i] {
                twice_counts += 1;
                break
            }
        }
        for i in 0..26 {
            if 3 == char_counts[i] {
                thrice_counts += 1;
                break
            }
        }
    }

    println!("{}", twice_counts * thrice_counts);
}

fn part_2(lines: &Vec<String>) {
    'outer: for (i, line) in lines[0..lines.len() - 1].iter().enumerate() {
        for other_line in lines[(i + 1)..].iter() {
            if 1 == distance(line, other_line) {
                println!("{}", common(line, other_line));
                break 'outer
            }
        }
    }
}

fn main() {
    let file = File::open("input").unwrap();
    let reader = BufReader::new(&file);
    let lines:Vec<String> = reader.lines().map (|l| l.unwrap()).collect();

    part_1(&lines);
    part_2(&lines);
}
