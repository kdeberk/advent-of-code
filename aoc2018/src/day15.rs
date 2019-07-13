use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::thread;
use std::time;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    pub fn d(&self, other: &Point) -> usize {
        ((self.x as isize - other.x as isize).abs() + (self.y as isize - other.y as isize).abs()) as usize
    }

    pub fn neighbors(&self) -> Vec<Point> {
        vec![
            Point { y: self.y - 1, x: self.x },
            Point { y: self.y    , x: self.x - 1},
            Point { y: self.y    , x: self.x + 1},
            Point { y: self.y + 1, x: self.x },
        ]
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Point) -> std::cmp::Ordering {
        if self.y == other.y {
            self.x.cmp(&other.x)
        } else {
            self.y.cmp(&other.y)
        }
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Point) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Race {
    Elf,
    Goblin,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Unit {
    race: Race,
    position: Point,
    hitpoints: u32,
    attack_power: u32,
}

impl Unit {
    fn goblin(position: Point) -> Self {
        Self { race: Race::Goblin, position: position, attack_power: 3, hitpoints: 200 }
    }

    fn elf(position: Point, attack_power: u32) -> Self {
        Self { race: Race::Elf, position: position, attack_power: attack_power, hitpoints: 200 }
    }

    pub fn alive(&self) -> bool {
        0 < self.hitpoints
    }
    
    pub fn round(&mut self, walls: &HashSet<Point>, other_units: &Vec<&RefCell<Unit>>) {
        if let Some(enemy) = self.find_weakest_adjacent_enemy(other_units) {
            self.attack(&mut enemy.borrow_mut())
        } else if let Some(next_position) = self.calculate_next_move(walls, other_units) {
           self.position = next_position;

            if let Some(enemy) = self.find_weakest_adjacent_enemy(other_units) {
                self.attack(&mut enemy.borrow_mut())
            }
        }
    }

    fn attack(&self, other: &mut Unit) {
        if self.attack_power >= other.hitpoints {
            other.hitpoints = 0;
        } else {
            other.hitpoints -= self.attack_power;
        }
    }

    fn find_weakest_adjacent_enemy<'a>(&self, other_units: &'a Vec<&RefCell<Unit>>) -> Option<&'a RefCell<Unit>> {
        let mut weakest_enemy:Option<&'a RefCell<Unit>> = None;

        for ref_enemy in other_units {
            if let Ok(enemy) = ref_enemy.try_borrow() {
                if enemy.race == self.race { continue }
                if ! enemy.alive() { continue }
                if ! self.can_attack(&enemy) { continue }

                if let Some(other_enemy) = weakest_enemy {
                    let other_enemy = other_enemy.borrow();

                    if other_enemy.hitpoints > enemy.hitpoints {
                        weakest_enemy = Some(ref_enemy)
                    } else if other_enemy.hitpoints == enemy.hitpoints && other_enemy.position > enemy.position {
                        weakest_enemy = Some(ref_enemy)
                    }
                } else {
                    weakest_enemy = Some(ref_enemy)
                }
            }
        }

        weakest_enemy
    }

    fn can_attack(&self, other: &Unit) -> bool {
        1 == self.position.d(&other.position)
    }

    fn calculate_next_move(&self, walls: &HashSet<Point>, other_units: &Vec<&RefCell<Unit>>) -> Option<Point> {
        let mut untraversable = walls.clone();
        let mut goal = HashSet::new();

        for other_unit in other_units {
            if let Ok(other_unit) = other_unit.try_borrow() {
                if ! other_unit.alive() { continue }

                if other_unit.race != self.race {
                    goal.insert(other_unit.position.clone());
                } else {
                    untraversable.insert(other_unit.position.clone());
                }
            }
        }
       
        Self::traversal(&self.position, &untraversable, &goal)
    }

    fn traversal(start: &Point, untraversable: &HashSet<Point>, goal: &HashSet<Point>) -> Option<Point> {
        let mut untraversable = untraversable.clone();
        let mut queue:VecDeque<(Point, Point, usize)> = VecDeque::new();

        for neighbor in start.neighbors() {
            if ! untraversable.contains(&neighbor) {
                queue.push_back((neighbor, neighbor, 1))
            }
        }

        let mut best_result:Option<(Point, Point, usize)> = None;

        while ! queue.is_empty() {
            if let Some((start, current, n_steps)) = queue.pop_front() {
                if untraversable.contains(&current) { continue } 

                if let Some((_, _, best_n_steps)) = best_result {
                    if n_steps > best_n_steps { break }
                }

                if goal.contains(&current) {
                    if let Some((_, other_current, other_n_steps)) = best_result {
                        if other_n_steps > n_steps || (other_n_steps == n_steps && other_current > current) {
                            best_result = Some((start, current, n_steps))
                        }
                    } else {
                        best_result = Some((start, current, n_steps))
                    }
                } else {
                    for neighbor in current.neighbors() {
                        if ! untraversable.contains(&neighbor) {
                            queue.push_back((start, neighbor, n_steps + 1));
                        }
                    }
                    untraversable.insert(current);
                }
            }
        }

        if let Some((best_start, _, _)) = best_result {
            Some(best_start)
        } else {
            None 
        }
    }
}

impl Ord for Unit {
    fn cmp(&self, other: &Unit) -> std::cmp::Ordering {
        self.position.cmp(&other.position)
    }
}

impl PartialOrd for Unit {
    fn partial_cmp(&self, other: &Unit) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::fmt::Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.race {
            Race::Elf => write!(f, "Elf({})@{}", self.hitpoints, self.position),
            Race::Goblin => write!(f, "Goblin({}){}", self.hitpoints, self.position),
        }
    }
}

#[derive(Clone)]
struct Game {
    width: usize,
    height: usize,
    walls: HashSet<Point>,
    elves: Vec<RefCell<Unit>>,
    goblins: Vec<RefCell<Unit>>,
}

impl Game {
    pub fn read_lines(lines: &Vec<String>, elves_attack_power: u32) -> Self {
        let mut walls = HashSet::new();
        let mut elves = vec![];
        let mut goblins = vec![];

        let mut size = (0, 0);

        for (y, line) in lines.iter().enumerate() {
            for (x, ch) in line.chars().enumerate() {
                match ch {
                    '#' => { walls.insert(Point { y: y, x: x}); },
                    'E' => elves.push(RefCell::new(Unit::elf(Point { y: y, x: x}, elves_attack_power))),
                    'G' => goblins.push(RefCell::new(Unit::goblin(Point { y: y, x: x}))),
                    '.' => (),
                    _ => panic!("Unknown character: {}", ch),
                }
                size = (y, x);
            }
        };

        Self { height: size.0 + 1, width: size.1 + 1, walls: walls, elves: elves, goblins: goblins }
    }

    pub fn over(&self) -> bool {
        0 == self.elves.len() || 0 == self.goblins.len()
    }

    pub fn turn(&mut self) -> bool {
        let mut full_round = true;

        { 
            let all_units = self.all_units();
            for unit in all_units.iter() {
                let mut unit = unit.borrow_mut();

                if ! unit.alive() { continue }

                let enemies = match unit.race {
                    Race::Elf => &self.goblins,
                    Race::Goblin => &self.elves,
                };

                if ! enemies.iter().any(|enemy| enemy.borrow().alive()) {
                    full_round = false;
                    break;
                } else {
                    unit.round(&self.walls, &all_units);
                }
            }
        }
        
        self.goblins.retain(|goblin| goblin.borrow().alive());
        self.elves.retain(|elf| elf.borrow().alive());

        full_round
    }

    pub fn hitpoints_left(&self) -> u32 {
        self.all_units().iter().fold(0, |acc, cur| acc + cur.borrow().hitpoints)
    }

    fn all_units<'a>(&'a self) -> Vec<&'a RefCell<Unit>> {
        let mut units = vec![];
        units.extend(&self.elves);
        units.extend(&self.goblins);
        units.sort();

        units
    }

    pub fn draw(&self) {
        let mut info:HashMap<usize, Vec<String>> = HashMap::new();
        let mut map = vec![vec!['.'; self.width]; self.height];

        println!("{}[2J", 27 as char);

        for wall in self.walls.iter() {
            map[wall.y][wall.x] = '#'
        }
        let units = self.all_units();

        for unit in units.iter() {
            let unit = unit.borrow();
            map[unit.position.y][unit.position.x] = match unit.race {
                Race::Elf => 'E',
                Race::Goblin => 'G',
            };

            info.entry(unit.position.y).or_insert(vec![]);
            info.get_mut(&unit.position.y).unwrap().push(format!("{}", unit));
        }

        for y in 0..self.height {
            for x in 0..self.width {
                print!("{}", map[y][x])
            }
            if let Some(strings) = info.get(&y) {
                print!("  {}", strings.join(", "))
            }
            println!("")
        }
        thread::sleep(time::Duration::from_millis(50))
    }
}

fn part_1(lines: &Vec<String>) -> u32 {
    let mut game = Game::read_lines(lines, 3);
    let mut n_full_rounds = 0;

    for round in 1.. {
        if env::var("DRAW").is_ok() {
            game.draw();
        }
        if ! game.turn() {
            n_full_rounds = round - 1;
            break
        } else if game.over() {
            n_full_rounds = round;
        }
    }

    n_full_rounds as u32 * game.hitpoints_left()
}


fn part_2(lines: &Vec<String>) -> Option<u32> {
    for attack_power in 4.. {
        let mut game = Game::read_lines(lines, attack_power);
        let n_elves = game.elves.len();

        let mut n_full_rounds = 0;
        for round in 1.. {
            if env::var("DRAW").is_ok() {
                game.draw()
            }

            if ! game.turn() {
                n_full_rounds = round - 1;
                break
            } else if game.over() {
                n_full_rounds = round;
                break
            } else if game.elves.len() != n_elves {
                break
            }
        }

        if game.elves.len() == n_elves {
            return Some(n_full_rounds as u32 * game.hitpoints_left());
        }
    }
    None
}


pub fn solve(lines: Vec<String>) {
    // TODO: move to tests
    // assert!(27730 == part_1("1_1.test"));
    // assert!(36334 == part_1("1_2.test"));
    // assert!(39514 == part_1("1_3.test"));
    // assert!(27755 == part_1("1_4.test"));
    // assert!(28944 == part_1("1_5.test"));
    // assert!(18740 == part_1("1_6.test"));

    // assert!(Some(4988) == part_2("2_1.test"));
    // assert!(Some(31284) == part_2("2_2.test"));
    // assert!(Some(3478) == part_2("2_3.test"));
    // assert!(Some(6474) == part_2("2_4.test"));
    // assert!(Some(1140) == part_2("2_5.test"));

    println!("Day 15");
    println!("Part 1: {}", part_1(&lines));
    println!("Part 2: {}", part_2(&lines).unwrap());
}
