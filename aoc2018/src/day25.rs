use std::collections::{HashMap, VecDeque};

#[derive(Eq, Hash, PartialEq)]
struct Point {
    i: usize,
    c: [isize; 4],
}

impl Point {
    fn from_line(i: usize, line: &str) -> Self {
        let c = line.trim().split(',').map(|part| part.parse::<isize>().unwrap()).collect::<Vec<isize>>();

        Point { i: i, c: [c[0], c[1], c[2], c[3]] }
    }

    fn d(a: &Point, b: &Point) -> usize {
        let mut d = 0;

        for (x, y) in a.c.iter().zip(b.c.iter()) {
            d += (x - y).abs() as usize;
        }

        d
    }
}

fn read_points(lines: Vec<String>) -> Vec<Point> {
    lines
        .iter()
        .enumerate()
        .map(|(i, line)| Point::from_line(i, line))
        .collect()
}

fn determine_neigbors<'a>(points: &'a Vec<Point>) -> HashMap<&'a Point, Vec<&'a Point>> {
    let mut neighbors:HashMap<&Point, Vec<&Point>> = HashMap::new();

    for point in points.iter() {
        neighbors.insert(&point, vec![]);
    }

    for (i, point) in points.iter().enumerate() {
        for other in points[(i + 1)..].iter() {
            if point == other { continue }
            if 3 < Point::d(&point, &other) { continue }

            neighbors.entry(&point).and_modify(|vec| vec.push(&other));
            neighbors.entry(&other).and_modify(|vec| vec.push(&point));
        }
    }

    neighbors
}

fn cascade_owner<'a>(owner: &'a Point, neighbors: &HashMap<&'a Point, Vec<&'a Point>>, point_to_owner:&mut HashMap<&'a Point, &'a Point>) {
    let mut queue:VecDeque<&Point> = VecDeque::new();

    for neighbor in neighbors.get(owner).unwrap() {
        queue.push_back(neighbor.clone());
    }

    while ! queue.is_empty() {
        let current = queue.pop_front().unwrap();

        if let Some(current_owner) = point_to_owner.get(&current) {
            if current_owner == &owner { continue }
        }

        point_to_owner.insert(current, owner);

        for neighbor in neighbors.get(current).unwrap() {
            queue.push_back(neighbor.clone());
        }
    }
}

fn part_1(points: &Vec<Point>) -> usize {
    let neighbors = determine_neigbors(points);
    let mut point_to_owner:HashMap<&Point, &Point> = HashMap::new();

    for point in points.iter() {
        point_to_owner.insert(&point, &point);
    }

    let mut count = 0;
    for point in points.iter() {
        let current_owner = point_to_owner.get(&point).unwrap().clone();

        if current_owner == point {
            cascade_owner(&point, &neighbors, &mut point_to_owner);
            count += 1;
        }
    }

    count
}

pub fn solve(lines: Vec<String>) {
    // assert!(2 == part_1(&read_points("test1")));
    // assert!(4 == part_1(&read_points("test2")));
    // assert!(3 == part_1(&read_points("test3")));
    // assert!(8 == part_1(&read_points("test4")));

    println!("Day 25");
    println!("Part 1: {}", part_1(&read_points(lines)));
}
