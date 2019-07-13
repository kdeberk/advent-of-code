use std::collections::{HashMap, VecDeque};

const TARGET:(usize, usize) = (785, 14);
const DEPTH:u32 = 4080;
const EROSION_MODULO:u32 = 20183;

const ROCKY:u32 = 0;
const WET:u32 = 1;
const NARROW:u32 = 2;

struct ErosionMap {
    target: (usize, usize),
    width: usize,
    height: usize,
    erosion: Vec<Vec<u32>>,
}

fn generate_erosion_map((max_y, max_x): (usize, usize), (target_y, target_x): (usize, usize), depth: u32) -> ErosionMap {
    let mut erosion = vec![vec![0u32; max_x + 1]; max_y + 1];

    for y in 1..max_y + 1 {
        erosion[y][0] = ((y * 48271) as u32 + depth) % EROSION_MODULO
    }
    for x in 1..max_x + 1 {
        erosion[0][x] = ((x * 16807) as u32 + depth) % EROSION_MODULO
    }

    for y in 1..max_y + 1 {
        for x in 1..max_x + 1 {
            if y == target_y && x == target_x {
                erosion[y][x] = 0;
            } else {
                erosion[y][x] = (depth + erosion[y - 1][x] * erosion[y][x - 1]) % EROSION_MODULO
            }
        }
    }

    ErosionMap { target: (target_y, target_x), width: max_x, height: max_y, erosion: erosion }
}

fn part_1(map: &ErosionMap) -> u32 {
    let mut risk = 0;

    for y in 0..map.target.0 + 1 {
        for x in 0..map.target.1 + 1 {
            risk += map.erosion[y][x] % 3;
        }
    }
    
    risk
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Tool {
    Torch,
    ClimbingGear,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Rescuer {
    y: usize,
    x: usize,
    minutes: u32,
    tool: Option<Tool>,
}

const COMPATIBLE_TOOL_AND_REGION:[(u32, Option<Tool>); 6] = [
    (ROCKY, Some(Tool::Torch)), (ROCKY, Some(Tool::ClimbingGear)),
    (WET, Some(Tool::ClimbingGear)), (WET, None),
    (NARROW, Some(Tool::Torch)), (NARROW, None),
];

impl Rescuer {
    fn possible_moves(&self, map: &ErosionMap) -> Vec<Self> {
        let mut moves = vec![];

        if 0 < self.y { moves.push(self.with_position(self.y - 1, self.x)) }
        if 0 < self.x { moves.push(self.with_position(self.y, self.x - 1)) }
        if self.y < (map.height - 1) { moves.push(self.with_position(self.y + 1, self.x)) }
        if self.x < (map.width - 1) { moves.push(self.with_position(self.y, self.x + 1)) }

        moves.push(self.with_tool(None));
        moves.push(self.with_tool(Some(Tool::Torch)));
        moves.push(self.with_tool(Some(Tool::ClimbingGear)));

        moves.into_iter().filter (|rescue| rescue.is_possible(map)).collect()
    }

    fn with_position(&self, y: usize, x: usize) -> Self {
        Self { x: x, y: y, minutes: self.minutes + 1, tool: self.tool }
    }

    fn with_tool(&self, tool: Option<Tool>) -> Self {
        Self { x: self.x, y: self.y, minutes: self.minutes + 7, tool: tool }
    }

    fn is_possible(&self, map: &ErosionMap) -> bool {
        COMPATIBLE_TOOL_AND_REGION.contains(&(map.erosion[self.y][self.x] % 3, self.tool))
    }
}

fn part_2(map: &ErosionMap) -> u32 {
    let mut scores:HashMap<(Option<Tool>, usize, usize), u32> = HashMap::new();
    let mut queue = VecDeque::new();

    queue.push_back(Rescuer { x: 0, y: 0, minutes: 0, tool: Some(Tool::Torch) });

    while ! queue.is_empty() {
        let rescuer = queue.pop_front().unwrap();

        if let Some(score) = scores.get(&(rescuer.tool, rescuer.y, rescuer.x)) {
            if *score <= rescuer.minutes {
                continue; // Won't improve this route
            }
        }
        scores.insert((rescuer.tool, rescuer.y, rescuer.x), rescuer.minutes);

        for possible_move in rescuer.possible_moves(map) {
            if let Some(score) = scores.get(&(possible_move.tool, possible_move.y, possible_move.x)) {
                if *score <= possible_move.minutes {
                    continue; // Won't improve this route
                }
            }
            queue.push_back(possible_move);
        }
    }

    *scores.get(&(Some(Tool::Torch), map.target.0, map.target.1)).unwrap()
}

pub fn solve(_lines: Vec<String>) {
    let test_erosion_map = generate_erosion_map((20, 20), (10, 10), 510);
    let real_erosion_map = generate_erosion_map((900, 50), TARGET, DEPTH);

    assert!(114 == part_1(&test_erosion_map));
    assert!(45 == part_2(&test_erosion_map));

    println!("Day 22");
    println!("Part 1: {}", part_1(&real_erosion_map));
    println!("Part 2: {}", part_2(&real_erosion_map));
}
