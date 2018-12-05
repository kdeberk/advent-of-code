
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashSet;

fn part_1(changes: &Vec<i32>) {
    let mut sum:i32 = 0;

    for change in changes.iter() {
        sum += change
    }

    println!("{}", sum);
}

fn part_2(changes: &Vec<i32>) {
    let mut sum:i32 = 0;
    let mut seen:HashSet<i32> = HashSet::new();

    for change in changes.iter().cycle() {
        sum += change;

        if seen.contains(&sum) {
            break
        } else {
            seen.insert(sum);
        }
    }

    println!("{}", sum);
}

fn main() {
    let file = File::open("input").unwrap();
    let reader = BufReader::new(&file);
    let changes = reader.lines().map(|line| line.unwrap().parse().unwrap()).collect();

    part_1(&changes);
    part_2(&changes);
}
