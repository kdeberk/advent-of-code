// TODO: move to shared module
#[cfg(debug_assertions)]
macro_rules! debug {
    ($($arg:tt)*) => { println!($($arg)*) }
}

#[cfg(not(debug_assertions))]
macro_rules! debug {
    ($($arg:tt)*) => { }
}

#[derive(Clone, PartialEq, Eq)]
enum Direction {
    NORTH,
    SOUTH,
    WEST,
    EAST,
}

#[derive(Clone, PartialEq, Eq)]
struct Cart {
    x: usize,
    y: usize,
    direction: Direction,
    n_turns: usize,
    crashed: bool,
}

impl Ord for Cart {
    fn cmp(&self, other: &Cart) -> std::cmp::Ordering {
        if self.y != other.y {
            self.y.cmp(&other.y)
        } else {
            self.x.cmp(&other.x)
        }
    }
}

impl PartialOrd for Cart {
    fn partial_cmp(&self, other: &Cart) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Cart {
    fn new(x: usize, y: usize, direction: Direction) -> Self {
        Cart { x: x, y: y, direction: direction, n_turns: 0, crashed: false }
    }

    fn step(&mut self) {
        match self.direction {
            Direction::NORTH => { self.y = self.y - 1; },
            Direction::SOUTH => { self.y = self.y + 1; },
            Direction::WEST => { self.x = self.x - 1; },
            Direction::EAST => { self.x = self.x + 1; },
        }
    }

    fn go_right(&mut self) {
        match self.direction {
            Direction::NORTH => { self.direction = Direction::EAST; },
            Direction::SOUTH => { self.direction = Direction::WEST; },
            Direction::WEST => { self.direction = Direction::NORTH; },
            Direction::EAST => { self.direction = Direction::SOUTH; },
        }
        self.step()
    }

    fn go_left(&mut self) {
        match self.direction {
            Direction::NORTH => { self.direction = Direction::WEST; },
            Direction::SOUTH => { self.direction = Direction::EAST; },
            Direction::WEST => { self.direction = Direction::SOUTH; },
            Direction::EAST => { self.direction = Direction::NORTH; },
        }
        self.step()
    }
}

fn read_input(lines: Vec<String>) -> (Vec<Vec<char>>, Vec<Cart>) {
    let mut map:Vec<Vec<char>> = lines.iter().map(|line| line.chars().collect()).collect();
    let mut carts = vec![];
    for (y, row) in map.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            match cell {
                '^' => carts.push(Cart::new(x, y, Direction::NORTH)),
                'v' => carts.push(Cart::new(x, y, Direction::SOUTH)),
                '>' => carts.push(Cart::new(x, y, Direction::EAST)),
                '<' => carts.push(Cart::new(x, y, Direction::WEST)),
                _ => (),
            }
        }
    }

    for cart in carts.iter() { 
        match cart.direction {
            Direction::NORTH | Direction::SOUTH => map[cart.y][cart.x] = '|',
            Direction::WEST | Direction::EAST => map[cart.y][cart.x] = '-',
        }
    }

    (map, carts)
}

fn move_carts(map: &Vec<Vec<char>>, carts: &mut Vec<Cart>) -> Vec<Cart> {
    let mut new_carts:Vec<Cart> = vec![];

    while 0 < carts.len() {
        let mut cart = carts.remove(0).clone();

        match map[cart.y][cart.x] {
            '/' => {
                match cart.direction {
                    Direction::NORTH | Direction::SOUTH => cart.go_right(),
                    Direction::WEST | Direction::EAST => cart.go_left(),
                }
            },
            '\\' => {
                match cart.direction {
                    Direction::NORTH | Direction::SOUTH => cart.go_left(),
                    Direction::WEST | Direction::EAST => cart.go_right(),
                }
            },
            '+' => {
                match cart.n_turns % 3 {
                    0 => cart.go_left(),
                    1 => cart.step(),
                    2 => cart.go_right(),
                    _ => (),
                }
                cart.n_turns += 1;
            }
            '-' | '|' => cart.step(),
            _ => panic!("Unknown sign: {}", map[cart.y][cart.x]),
        }

        for other_cart in carts.iter_mut() {
            if cart.x == other_cart.x && cart.y == other_cart.y {
                debug!("Collision at {},{}", cart.x, cart.y);
                cart.crashed = true;
                other_cart.crashed = true;
            }
        }

        for other_cart in new_carts.iter_mut() {
            if cart.x == other_cart.x && cart.y == other_cart.y {
                debug!("Collision at {},{}", cart.x, cart.y);
                cart.crashed = true;
                other_cart.crashed = true;
            }
        }
        new_carts.push(cart);
    }
    new_carts.sort();
    new_carts
}

fn part_1(map: &Vec<Vec<char>>, carts: &Vec<Cart>) -> (usize, usize) {
    let mut carts = carts.clone();

    for _ in 0.. {
        carts = move_carts(map, &mut carts);
        if carts.iter().any (|cart| cart.crashed) {
            break
        }
    }
    
    let crashed_cart = carts.iter().find (|cart| cart.crashed).unwrap();
    (crashed_cart.x, crashed_cart.y)
}

fn part_2(map: &Vec<Vec<char>>, carts: &Vec<Cart>) -> (usize, usize) {
    let mut carts = carts.clone();

    for _ in 0.. {
        carts = move_carts(map, &mut carts).into_iter().filter(|cart| ! cart.crashed ).collect();
        if 1 == carts.len() {
            break
        }
    }
    (carts[0].x, carts[0].y)
}


pub fn solve(lines: Vec<String>) {
    let (map, carts) = read_input(lines);

    println!("Day 13");
    println!("Part 1: {:?}", part_1(&map, &carts));
    println!("Part 2: {:?}", part_2(&map, &carts));
}
