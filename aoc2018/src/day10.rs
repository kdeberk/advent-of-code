#[cfg(debug_assertions)]
macro_rules! debug {
    ($($arg:tt)*) => { println!($($arg)*) }
}

#[cfg(not(debug_assertions))]
macro_rules! debug {
    ($($arg:tt)*) => { }
}

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

fn read_points(lines: Vec<String>) -> Vec<Point> {
    lines.iter().map(|line| {
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
        debug!("Cowardly refusing to draw a {}x{} screen", height, width);
        false
    } else {
        let mut map = vec![vec!['.'; width]; height];

        for point in points {
            map[(point.y - min_y) as usize][(point.x - min_x) as usize] = '#'
        }

        println!("Part 1:");
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
            println!("Part 2: {}", time);

            drew_before = true;
        } else if drew_before {
            break;
        }

        for point in points.iter_mut() {
            point._move();
        }
    }
}


pub fn solve(lines: Vec<String>) {
    let points = read_points(lines);

    println!("Day 10");
    part_1(&points);
}
