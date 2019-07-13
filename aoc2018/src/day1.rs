use std::collections::HashSet;

fn part_1(changes: &Vec<i32>) -> i32 {
    changes.iter().fold(0, |sum, change| sum + change)
}

fn part_2(changes: &Vec<i32>) -> i32 {
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

    sum
}

pub fn solve(lines: Vec<String>) {
    let changes = lines.iter().map(|line| line.parse().unwrap()).collect();

    println!("Day 1");
    println!("Part 1: {}", part_1(&changes));
    println!("Part 2: {}", part_2(&changes));
}
