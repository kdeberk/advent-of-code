use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::ops::Range;
use std::thread;
use std::time;

#[derive(Eq, Hash, PartialEq)]
enum Direction {
    Down,
    Left,
    Right,
}

enum Water {
    Still,
    Flowing(Direction),
}

struct Grid {
    clay: HashSet<(isize, isize)>,
    min_x: isize,
    max_x: isize,
    min_y: isize,
    max_y: isize,
}

fn gib_clay(lines: Vec<String>) -> Grid {
    let mut clay = HashSet::new();

    for line in lines {
        let parts = line.split([' ', '=', ',', '.'].as_ref()).collect::<Vec<&str>>();

        match parts[0] {
            "x" => {
                let x:isize = parts[1].parse().unwrap();
                let yr:Range<isize> = parts[4].parse().unwrap()..(parts[6].parse::<isize>().unwrap() + 1);
                for y in yr {
                    clay.insert((y, x));
                }
            },
            "y" => {
                let y:isize = parts[1].parse().unwrap();
                let xr:Range<isize> = parts[4].parse().unwrap()..(parts[6].parse::<isize>().unwrap() + 1);
                for x in xr {
                    clay.insert((y, x));
                }
            },
            _ => (),
        }
    }

    let mut min_x = std::isize::MAX;
    let mut max_x = std::isize::MIN;
    let mut min_y = std::isize::MAX;
    let mut max_y = std::isize::MIN;
    for (y, x) in clay.iter() {
        if *x < min_x { min_x = *x }
        if *x > max_x { max_x = *x }
        if *y < min_y { min_y = *y }
        if *y > max_y { max_y = *y }
    }

    Grid { clay: clay, min_x: min_x - 1, max_x: max_x + 1, min_y: min_y, max_y: max_y }
}

fn draw_grid(grid: &Grid, water: &HashMap<(isize, isize), Water>) {
    for y in 0..(grid.max_y + 1) {
        for x in grid.min_x..(grid.max_x + 1) {
            if grid.clay.contains(&(y, x)) {
                print!("#")
            } else {
                match water.get(&(y, x)) {
                    Some(Water::Still) => print!("~"),
                    Some(Water::Flowing(_)) => print!("|"),
                    None => print!("."),
                }
            }
        }
        println!("")
    }
    thread::sleep(time::Duration::from_millis(200))
}

fn calculate_water(grid: &Grid) -> HashMap<(isize, isize), Water> {
    let mut stack:VecDeque<(isize, isize)> = VecDeque::new();
    let mut water:HashMap<(isize, isize), Water> = HashMap::new();

    stack.push_back((grid.min_y, 500));
    water.insert((grid.min_y, 500), Water::Flowing(Direction::Down));

    while ! stack.is_empty() {
        let current = stack.back().unwrap().clone();
        if current.0 >= grid.max_y {
            stack.pop_back();
            continue;
        }

        let down = (current.0 + 1, current.1);
        let can_flow_down = ! grid.clay.contains(&down);
        let did_flow_down = water.contains_key(&down);
        let down_is_still = if let Some(Water::Still) = water.get(&down) { true } else { false };

        let left = (current.0, current.1 - 1);
        let can_flow_left = ! grid.clay.contains(&left) && grid.min_x <= left.1;
        let did_flow_left = water.contains_key(&left);
        let left_is_still = if let Some(Water::Still) = water.get(&left) { true } else { false };

        let right = (current.0, current.1 + 1);
        let can_flow_right = ! grid.clay.contains(&right) && right.1 <= grid.max_x;
        let did_flow_right = water.contains_key(&right);
        let right_is_still = if let Some(Water::Still) = water.get(&right) { true } else { false };

        match water.get(&current).unwrap() {
            Water::Flowing(Direction::Down) => {
                if ! can_flow_down || (did_flow_down && down_is_still) {
                    if did_flow_left || did_flow_right {
                        if (! did_flow_left || left_is_still) && (! did_flow_right || right_is_still) {
                            water.insert(current, Water::Still);
                        } else if did_flow_left && ! left_is_still && right_is_still {
                            for x in (current.1 + 1).. {
                                if let Some(Water::Still) = water.get(&(current.0, x)) {
                                    water.insert((current.0, x), Water::Flowing(Direction::Left));
                                } else {
                                    break;
                                }
                            }
                        } else if did_flow_right && ! right_is_still && left_is_still {
                            for x in (std::isize::MIN..current.1).rev() {
                                if let Some(Water::Still) = water.get(&(current.0, x)) {
                                    water.insert((current.0, x), Water::Flowing(Direction::Left));
                                } else {
                                    break;
                                }
                            }
                        }
                        stack.pop_back();
                    } else if ! can_flow_left && ! can_flow_right {
                        water.insert(current, Water::Still);
                        stack.pop_back();
                    } else {
                        if can_flow_left {
                            water.insert(left, Water::Flowing(Direction::Left));
                            stack.push_back(left);
                        }
                        if can_flow_right {
                            water.insert(right, Water::Flowing(Direction::Right));
                            stack.push_back(right);
                        }
                    }
                } else if did_flow_down {
                    stack.pop_back();
                } else if can_flow_down {
                    water.insert(down, Water::Flowing(Direction::Down));
                    stack.push_back(down);
                } else {
                    water.insert(current, Water::Still);
                    stack.pop_back();
                }
            },
            Water::Flowing(Direction::Left) => {
                if ! can_flow_down || (did_flow_down && down_is_still) {
                    if did_flow_left {
                        if left_is_still {
                            water.insert(current, Water::Still);
                        }
                        stack.pop_back();
                    } else if ! can_flow_left {
                        water.insert(current, Water::Still);
                        stack.pop_back();
                    } else if can_flow_left {
                        water.insert(left, Water::Flowing(Direction::Left));
                        stack.push_back(left);
                    }
                } else if did_flow_down {
                    stack.pop_back();
                } else if can_flow_down {
                    water.insert(down, Water::Flowing(Direction::Down));
                    stack.push_back(down);
                } else {
                    water.insert(current, Water::Still);
                    stack.pop_back();
                }
            },
            Water::Flowing(Direction::Right) => {
                if ! can_flow_down || (did_flow_down && down_is_still) {
                    if did_flow_right {
                        if right_is_still {
                            water.insert(current, Water::Still);
                        }
                        stack.pop_back();
                    } else if ! can_flow_right {
                        water.insert(current, Water::Still);
                        stack.pop_back();
                    } else if can_flow_right {
                        water.insert(right, Water::Flowing(Direction::Right));
                        stack.push_back(right);
                    }
                } else if did_flow_down {
                    stack.pop_back();
                } else if can_flow_down {
                    water.insert(down, Water::Flowing(Direction::Down));
                    stack.push_back(down);
                } else {
                    water.insert(current, Water::Still);
                    stack.pop_back();
                }
            },
            Water::Still => {
                stack.pop_back();
            }
        }
    }
    water
}

fn part_1(grid: &Grid) -> usize {
    let water = calculate_water(grid);
    if env::var("DRAW").is_ok() { draw_grid(&grid, &water); }
    water.len()
}

fn part_2(grid: &Grid) -> isize {
    let mut count = 0;
    for (_, v) in calculate_water(grid) {
        if let Water::Still = v { count += 1 };
    }
    count
}

pub fn solve(lines: Vec<String>) {
    let grid = gib_clay(lines);

    println!("Day 17");
    println!("Part 1: {}", part_1(&grid));
    println!("Part 2: {}", part_2(&grid));
}
