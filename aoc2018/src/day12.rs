use std::collections::HashSet;
use std::ops::Range;

const PADDING:isize = 2;
const SPREAD_RANGE:Range<isize> = (0 - PADDING)..(PADDING + 1);

fn parse_state(state: &str) -> HashSet<isize> {
    let mut result = HashSet::new();

    for (i, ch) in state.chars().enumerate() {
        if '#' == ch {
            result.insert(i as isize);
        }
    }

    result
}

fn parse_notes(note_lines: Vec<&str>) -> HashSet<Vec<bool>> {
    let mut result = HashSet::new();

    for line in note_lines {
        let parts:Vec<&str> = line.split(" => ").collect();

        if "#" == parts[1] {
            result.insert(parts[0].chars().map(|ch| '#' == ch).collect());
        }
    }

    result
}

fn generate_next_state(state: &HashSet<isize>, notes: &HashSet<Vec<bool>>) -> HashSet<isize> {
    let mut next_state:HashSet<isize> = HashSet::new();

    for i in state.iter() {
        for j in SPREAD_RANGE {
            let surroundings:Vec<bool> = SPREAD_RANGE.map(|k| state.contains(&(i + j + k))).collect();

            if notes.contains(&surroundings) {
                next_state.insert(i + j);
            }
        }
    }

    next_state
}

fn plants_can_influence_each_other(state: &HashSet<isize>) -> bool {
    // if all plants are at a too large distance from each other, they will all follow the same rule
    // and hence move into the same direction
    // the minimum distance between plants needs to be greater than 2 * PADDING

    let mut min_distance = std::isize::MAX;

    for i in state.iter() {
        for j in state.iter() {
            if *i != *j && min_distance > (i - j).abs() {
                min_distance = (i - j).abs()
            }
        }
    }
    min_distance <= 2 * PADDING
}

fn predict_movement(state: &HashSet<isize>, next_state: &HashSet<isize>) -> isize {
    let next_lowest = next_state.iter().fold(std::isize::MAX, |cur, min| isize::min(cur, *min));
    let current_lowest = state.iter().fold(std::isize::MAX, |cur, min| isize::min(cur, *min));

    next_lowest - current_lowest
}

fn part_1(initial: &HashSet<isize>, notes: &HashSet<Vec<bool>>, n_iterations: usize) -> isize {
    let mut state = initial.clone();

    for iter in 0..n_iterations {
        state = generate_next_state(&state, notes);

        if ! plants_can_influence_each_other(&state) {
            let movement = predict_movement(&state, &generate_next_state(&state, notes));
            let current_sum = state.iter().fold(0, |cur, acc| cur + acc) as isize;

            return (n_iterations - iter - 1) as isize * state.len() as isize * movement + current_sum;
        }
    }

    state.iter().fold(0, |cur, acc| cur + acc)
}


pub fn solve(_lines: Vec<String>) {
    let notes = parse_notes(
        vec![
            ".#### => .", ".###. => .", "#.... => .", "##### => .", "..### => #", "####. => #", "..#.. => .", "###.# => .",
            "..##. => .", "#.##. => #", "#.#.. => .", "##... => .", "..#.# => #", "#.### => #", ".#..# => .", "#...# => #",
            ".##.# => #", ".#.#. => #", "#..#. => #", "###.. => #", "...#. => .", ".#.## => #", ".##.. => .", "#..## => .",
            "##.## => .", ".#... => #", "#.#.# => .", "##..# => .", "....# => .", "..... => .", "...## => #", "##.#. => .",
        ]);

    let initial = parse_state("#.#..#.##.#..#.#..##.######...####.........#..##...####.#.###......#.#.##..#.#.###.#..#.#.####....##");

    println!("Day 12");
    println!("Part 1: {}", part_1(&initial, &notes, 20));
    println!("Part 2: {}", part_1(&initial, &notes, 50_000_000_000));
}
