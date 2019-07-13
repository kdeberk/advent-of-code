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

fn part_1(chars: &Vec<char>) -> usize {
    reduce(chars.clone()).len()
}

fn part_2(chars: &Vec<char>) -> usize {
    let mut best_length = chars.len();

    for c in "abcdefghijklmnopqrstuvwxyz".chars() {
        let length = reduce(remove_all_of(&chars, c)).len();

        if best_length > length {
            best_length = length;
        }
    }

    best_length
}

pub fn solve(lines: Vec<String>) {
    let string = lines.get(0).unwrap();
    let chars:Vec<char> = string.chars().collect();

    println!("Day 5");
    println!("Part 1: {}", part_1(&chars));
    println!("Part 2: {}", part_2(&chars));
}
