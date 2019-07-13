const SIZE:usize = 1000;
const FREE:i32 = 0;
const CONFLICT:i32 = -1;

struct Rectangle {
    id: i32,
    x: usize,
    y: usize,
    width: usize,
    height: usize,
}

impl Rectangle {
    fn read(line: &str) -> Self {
        let args:Vec<&str> = line.split(['#', ',', '@', 'x', ':'].as_ref()).map (|part| part.trim()).collect();

        Rectangle {
            id: args[1].parse().unwrap(),
            x: args[2].parse().unwrap(),
            y: args[3].parse().unwrap(),
            width: args[4].parse().unwrap(),
            height: args[5].parse().unwrap(),
        }
    }
}

fn part_1(rectangles: &Vec<Rectangle>) -> i32 {
    let mut map:[[i32; SIZE]; SIZE] = [[FREE; SIZE]; SIZE];

    for rect in rectangles.iter() {
        for x in rect.x..(rect.x + rect.width) {
            for y in rect.y..(rect.y + rect.height) {
                if FREE == map[x][y] {
                    map[x][y] = rect.id;
                } else {
                    map[x][y] = CONFLICT;
                }
            }
        }
    }

    let mut count = 0;
    for x in 0..SIZE {
        for y in 0..SIZE {
            if CONFLICT == map[x][y] {
                count += 1;
            }
        }
    }

    count
}


fn part_2(rectangles: &Vec<Rectangle>) -> Option<i32> {
    let mut map:[[i32; SIZE]; SIZE] = [[FREE; SIZE]; SIZE];

    for rect in rectangles.iter() {
        for x in rect.x..(rect.x + rect.width) {
            for y in rect.y..(rect.y + rect.height) {
                if FREE == map[x][y] {
                    map[x][y] = rect.id;
                } else {
                    map[x][y] = CONFLICT;
                }
            }
        }
    }

    for rect in rectangles.iter() {
        let mut overlaps = false;

        'x: for x in rect.x..(rect.x + rect.width) {
            for y in rect.y..(rect.y + rect.height) {
                if rect.id != map[x][y] {
                    overlaps = true;
                    break 'x
                }
            }
        }

        if ! overlaps {
            return Some(rect.id);
        }
    }

    None
}


pub fn solve(lines: Vec<String>) {
    let rectangles:Vec<Rectangle> = lines.iter().map (|line| Rectangle::read(&line)).collect();

    println!("Day 3");
    println!("Part 1: {}", part_1(&rectangles));
    println!("Part 2: {}", part_2(&rectangles).unwrap());
}
