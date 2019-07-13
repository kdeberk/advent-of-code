use std::collections::{HashMap, HashSet};

#[derive(Debug, Eq, PartialEq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn read(line: &str) -> Self {
        let parts:Vec<isize> = line.split(",").map(|p| p.trim().parse().unwrap()).collect();

        Self { x: parts[1], y: parts[0] }
    }

    fn d(&self, other: &Point) -> usize {
        ((self.x - other.x).abs() + (self.y - other.y).abs()) as usize
    }
}


fn boundaries(points: &Vec<Point>) -> (Point, Point) {
    let mut lowest = Point { x: std::isize::MAX, y: std::isize::MAX };
    let mut highest = Point { x: std::isize::MIN, y: std::isize::MIN };

    for point in points {
        if point.x > highest.x {
            highest.x = point.x
        }
        if point.x < lowest.x {
            lowest.x = point.x
        }

        if point.y > highest.y {
            highest.y = point.y
        }
        if point.y < lowest.y {
            lowest.y = point.y
        }
    }

    (lowest, highest)
}

fn belongs_to<'a>(coord: &Point, points: &'a Vec<Point>) -> Option<&'a Point> {
    let mut closest = vec![&points[0]];

    for point in points[1..].iter() {
        if coord.d(&point) < coord.d(&closest[0]) {
            closest.clear();
            closest.push(point);
        } else if coord.d(&point) == coord.d(&closest[0]) {
            closest.push(point);
        }
    }

    if 1 == closest.len() {
        return Some(closest[0])
    } else {
        return None
    }
}


fn part_1(points: &Vec<Point>) -> u32 {
    let (lowest, highest) = boundaries(points);
    let mut counts:HashMap<&Point, u32> = HashMap::new();

    for x in (lowest.x - 1)..(highest.x + 1) {
        for y in (lowest.y - 1)..(highest.y + 1) {
            if let Some(closest) = belongs_to(&Point { x: x, y: y }, points) {
                let count = counts.entry(closest).or_insert(0);
                *count += 1;
            }
        }
    }

    let mut boundary_points:HashSet<&Point> = HashSet::new();
    for x in (lowest.x - 1)..(highest.x + 1) {
        if let Some(closest) = belongs_to(&Point { x: x, y: lowest.y - 1 }, points) {
            boundary_points.insert(closest);
        }
        if let Some(closest) = belongs_to(&Point { x: x, y: highest.y + 1 }, points) {
            boundary_points.insert(closest);
        }
    }
    for y in (lowest.y - 1)..(highest.y + 1) {
        if let Some(closest) = belongs_to(&Point { x: lowest.x - 1, y: y }, points) {
            boundary_points.insert(closest);
        }
        if let Some(closest) = belongs_to(&Point { x: highest.x + 1, y: y }, points) {
            boundary_points.insert(closest);
        }
    }

    let mut highest_count = 0;
    for (point, count) in counts {
        if ! boundary_points.contains(point) {
            if count > highest_count {
                highest_count = count;
            }
        }
    }

    highest_count
}

const LIMIT:usize = 10_000;

fn part_2(points: &Vec<Point>) -> usize {
    let (lowest, highest) = boundaries(points);
    
    let mut counts = 0;
    for x in (lowest.x)..(highest.x) {
        for y in (lowest.y)..(highest.y) {
            let mut sum = 0;
            let coord = Point { x: x, y: y };

            for point in points {
                sum += coord.d(point);
            }

            if LIMIT > sum {
                counts += 1;
            }
        }
    }

    counts
}


pub fn solve(lines: Vec<String>) {
    let points = lines.iter().map (|l| Point::read(&l)).collect();

    println!("Day 6");
    println!("Part 1: {}", part_1(&points));
    println!("Part 2: {}", part_2(&points));
}
