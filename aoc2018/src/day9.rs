use std::collections::VecDeque;

fn rotate<T>(i: isize, circle: &mut VecDeque<T>) {
    if 0 < i {
        for _ in 0..i {
            let value = circle.pop_front().unwrap();
            circle.push_back(value);
        }
    } else if 0 > i {
        for _ in 0..i.abs() {
            let value = circle.pop_back().unwrap();
            circle.push_front(value);
        }
    }
}

fn part_1(n_players: usize, highest_marble: usize) -> usize {
    let mut scores = vec![0usize; n_players];
    let mut circle:VecDeque<usize> = VecDeque::new();

    circle.push_back(0);
    for marble in 1..(highest_marble + 1) {
        if 0 == marble % 23 {
            rotate(-7, &mut circle);

            scores[(marble - 1) % n_players] += marble;
            scores[(marble - 1) % n_players] += circle.pop_back().unwrap();
            rotate(1, &mut circle);
        } else {
            rotate(1, &mut circle);
            circle.push_back(marble);
        }
    }

    scores.into_iter().fold(std::usize::MIN, usize::max)
}


pub fn solve(_lines: Vec<String>) {
    println!("Day 9");
    println!("Part 1: {}", part_1(429, 70901));
    println!("Part 2: {}", part_1(429, 7090100));
}
