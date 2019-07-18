fn distance(str_a: &str, str_b: &str) -> usize {
    str_a
        .chars()
        .zip(str_b.chars())
        .filter(|(char_a, char_b)| char_a != char_b)
        .count()
}

fn common_chars(str_a: &str, str_b: &str) -> String {
    str_a
        .chars()
        .zip(str_b.chars())
        .filter(|(char_a, char_b)| char_a == char_b)
        .map(|(char_a, _char_b)| char_a)
        .collect()
}

fn part_1(lines: &Vec<String>) -> i32 {
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

    twice_counts * thrice_counts
}

fn part_2(lines: &Vec<String>) -> Option<String> {
    for (i, line) in lines[0..lines.len() - 1].iter().enumerate() {
        for other_line in lines[(i + 1)..].iter() {
            if 1 == distance(line, other_line) {
                return Some(common_chars(line, other_line))
            }
        }
    }
    None
}

pub fn solve(lines: Vec<String>) {
    println!("Day 2");
    println!("Part 1: {}", part_1(&lines));
    println!("Part 2: {}", part_2(&lines).unwrap());
}
