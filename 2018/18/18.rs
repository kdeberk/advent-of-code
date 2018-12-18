use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::thread;
use std::time;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Acre {
    Open,
    Tree,
    Lumberyard,
}

#[derive(Clone, Eq, Hash, PartialEq)]
struct Acres {
    acres: Vec<Vec<Acre>>,
    width: usize,
    height: usize,
}

impl Acres {
    pub fn read_from_file(filename: &str) -> Self {
        let file = File::open(filename).unwrap();
        let reader = BufReader::new(&file);
        let mut acres = vec![];

        for line in reader.lines() {
            let line = line.unwrap();
            let mut row = vec![];

            for ch in line.chars() {
                match ch {
                    '|' => { row.push(Acre::Tree) },
                    '.' => { row.push(Acre::Open) },
                    '#' => { row.push(Acre::Lumberyard) },
                    _ => panic!("Unknown character: {}", ch),
                }
            }
            acres.push(row);
        }

        let height = acres.len();
        let width = acres[0].len();

        Self { acres: acres, height: height, width: width }
    }


    pub fn draw(&self) {
        for row in self.acres.iter() {
            for acre in row.iter() {
                match acre {
                    Acre::Tree => print!("|"),
                    Acre::Open => print!("."),
                    Acre::Lumberyard => print!("#"),
                }
            }
            println!("")
        }
        thread::sleep(time::Duration::from_millis(10))
    }

    pub fn next(&self) -> Self {
        let mut next_acres = vec![];

        for (y, row) in self.acres.iter().enumerate() {
            let mut next_row = vec![];

            for (x, acre) in row.iter().enumerate() {
                let next_acre = match acre {
                    Acre::Open => {
                        if 3 <= self.count_neighbors((y, x), Acre::Tree) { Acre::Tree } else { Acre::Open }
                    },
                    Acre::Tree => {
                        if 3 <= self.count_neighbors((y, x), Acre::Lumberyard) { Acre::Lumberyard } else { Acre::Tree }
                    },
                    Acre::Lumberyard => {
                        if 0 < self.count_neighbors((y, x), Acre::Lumberyard) && 0 < self.count_neighbors((y, x), Acre::Tree) {
                            Acre::Lumberyard
                        } else {
                            Acre::Open
                        }
                    },
                };
                next_row.push(next_acre);
            }
            next_acres.push(next_row);
        }

        Self { acres: next_acres, height: self.height, width: self.width }
    }

    pub fn score(&self) -> usize {
        let mut n_lumberjacks = 0;
        let mut n_trees = 0;

        for row in self.acres.iter() {
            for acre in row.iter() {
                match acre {
                    Acre::Tree => { n_trees += 1},
                    Acre::Lumberyard => { n_lumberjacks += 1 },
                    _ => (),
                }
            }
        }

        n_lumberjacks * n_trees
    }

    fn count_neighbors(&self, (y, x): (usize, usize), acre: Acre) -> usize {
        let mut count = 0;

        let ystart = isize::max(0, y as isize - 1) as usize;
        let yend = usize::min(self.height, y + 2);
        let xstart = isize::max(0, x as isize - 1) as usize;
        let xend = usize::min(self.width, x + 2);

        for ny in ystart..yend {
            for nx in xstart..xend {
                if x == nx && y == ny {
                    continue
                } else if self.acres[ny][nx] == acre {
                    count += 1;
                }
            }
        }
        count
    }
}

fn part_1(filename: &str) -> usize {
    let mut acres = Acres::read_from_file(filename);

    for _ in 0..10 {
        if env::var("TRIPPINGBALLS").is_ok() { acres.draw(); }
        acres = acres.next()
    }
    acres.score()
}

fn acres_at_minute(acres_to_minute:HashMap<Acres, usize>, minute: usize) -> Option<Acres> {
    for (k, v) in acres_to_minute {
        if v == minute {
            return Some(k)
        }
    }
    None
}

fn part_2(filename: &str) -> usize {
    let limit = 10_000_000_000;
    let mut acres = Acres::read_from_file(filename);
    let mut seen:HashMap<Acres, usize> = HashMap::new();

    let mut minute = 0;
    while limit > minute {
        acres = acres.next();

        if env::var("TRIPPINGBALLS").is_ok() {
            acres.draw();
            minute += 1
        } else if seen.contains_key(&acres) {
            break
        } else {
            seen.insert(acres.clone(), minute);
            minute += 1
        }
    }

    let prev_minute = seen.remove(&acres).unwrap();
    let range = minute - prev_minute - 1;
    let limit_minute = prev_minute + ((limit - minute) % range);
    acres_at_minute(seen, limit_minute).unwrap().score()
}

fn main() {
    println!("{}", part_1("input"));
    println!("{}", part_2("input"));
}
