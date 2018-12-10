use std::fs::File;
use std::io::{BufRead, BufReader};
use std::thread;
use std::time;

#[derive(Clone, Debug)]
struct Point {
    x: isize,
    y: isize,
    dx: isize,
    dy: isize,
}

impl Point {
    fn _move(&mut self) {
        self.x += self.dx;
        self.y += self.dy;
    }
}

fn read_points(filename: &str) -> Vec<Point> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(&file);

    reader.lines().map(|line| {
        let line = line.unwrap();
        let parts:Vec<isize> = line
            .split(['<', '>', ','].as_ref())
            .map(|part| part.trim().parse::<isize>())
            .filter(|result| result.is_ok())
            .map(|result| result.unwrap())
            .collect();

        Point { x: parts[0], y: parts[1], dx: parts[2], dy: parts[3] }
    }).collect()
}

const LIMIT:usize = 70;

fn draw(points: &Vec<Point>) -> bool {
    let min_x:isize = points.iter().map(|point| point.x).fold(std::isize::MAX, isize::min);
    let max_x:isize = points.iter().map(|point| point.x).fold(std::isize::MIN, isize::max);
    let min_y:isize = points.iter().map(|point| point.y).fold(std::isize::MAX, isize::min);
    let max_y:isize = points.iter().map(|point| point.y).fold(std::isize::MIN, isize::max);

    let width:usize = 1 + (max_x - min_x) as usize;
    let height:usize = 1 + (max_y - min_y) as usize;

    if width > LIMIT || height > LIMIT {
        println!("Cowardly refusing to draw a {}x{} screen", height, width);
        false
    } else {
        let mut map = vec![vec!['.'; width]; height];

        for point in points {
            map[(point.y - min_y) as usize][(point.x - min_x) as usize] = '#'
        }

        for y in 0..height {
            for x in 0..width {
                print!("{}", map[y][x]);
            }
            println!("")
        }
        true
    }
}

fn part_1(points: &Vec<Point>) {
    let mut drew_before = false;
    let mut points = points.clone();

    for time in 0.. {
        if draw(&points) {
            println!("{}", time);

            thread::sleep(time::Duration::from_secs(1));
            drew_before = true;
        } else if drew_before {
            break;
        }

        for point in points.iter_mut() {
            point._move();
        }
    }
}


fn main() {
    let points = read_points("input");

    part_1(&points);
}
