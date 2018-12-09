
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

fn part_1(n_players: usize, highest_marble: usize) {
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
    println!("{}", scores.into_iter().fold(std::usize::MIN, usize::max))
}


fn main() {
    part_1(429, 70901);
    part_1(429, 7090100);
}
