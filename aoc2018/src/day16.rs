use std::collections::{HashMap, HashSet, VecDeque};

const ALL_KNOWN_OPCODES:[&str; 16] = ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"];

#[derive(Eq, Clone, Debug, PartialEq)]
struct Machine {
    registers: [u32; 4],
}

impl Machine {
    fn new() -> Self {
        Self { registers: [0u32; 4] }
    }

    fn from_line(line: &str) -> Self {
        let line = &line[1..line.len() - 1];
        let values = line
            .split(", ")
            .map(|str| str.parse::<u32>().unwrap())
            .collect::<Vec<u32>>();

        let mut registers = [0u32; 4];
        for (i, value) in values.into_iter().enumerate() {
            registers[i] = value;
        }

        Self { registers: registers }
    }

    fn execute(&mut self, instruction: &Instruction) {
        let a = instruction.a;
        let b = instruction.b;
        let ra = instruction.a as usize;
        let rb = instruction.b as usize;
        let rc = instruction.c as usize;

        match instruction.opcode.as_str() {
            "addr" => { self.registers[rc] = self.registers[ra] + self.registers[rb] },
            "addi" => { self.registers[rc] = self.registers[ra] + b },
            "mulr" => { self.registers[rc] = self.registers[ra] * self.registers[rb] },
            "muli" => { self.registers[rc] = self.registers[ra] * b },
            "banr" => { self.registers[rc] = self.registers[ra] & self.registers[rb] },
            "bani" => { self.registers[rc] = self.registers[ra] & b },
            "borr" => { self.registers[rc] = self.registers[ra] | self.registers[rb] },
            "bori" => { self.registers[rc] = self.registers[ra] | b },
            "setr" => { self.registers[rc] = self.registers[ra] },
            "seti" => { self.registers[rc] = a },
            "gtir" => { self.registers[rc] = if a > self.registers[rb] { 1 } else { 0 } },
            "gtri" => { self.registers[rc] = if self.registers[ra] > b { 1 } else { 0 } },
            "gtrr" => { self.registers[rc] = if self.registers[ra] > self.registers[rb] { 1 } else { 0 } },
            "eqir" => { self.registers[rc] = if a == self.registers[rb] { 1 } else { 0 } },
            "eqri" => { self.registers[rc] = if self.registers[ra] == b { 1 } else { 0 } },
            "eqrr" => { self.registers[rc] = if self.registers[ra] == self.registers[rb] { 1 } else { 0 } },
            _ => panic!("Unknown instruction: {}", instruction.opcode),
        }
    }
}

#[derive(Debug)]
struct Instruction {
    opcode: String,
    a: u32,
    b: u32,
    c: u32
}

#[derive(Debug)]
struct UnknownInstruction {
    opcode: u8,
    a: u32,
    b: u32,
    c: u32
}

impl UnknownInstruction {
    fn from_line(line: &str) -> Self {
        let operands:Vec<u32> = line.split(" ").map(|digit| digit.parse::<u32>().unwrap()).collect();

        Self { opcode: operands[0] as u8, a: operands[1], b: operands[2], c: operands[3] }
    }

    fn set_opcode(&self, opcode: &str) -> Instruction {
        Instruction { opcode: String::from(opcode), a: self.a, b: self.b, c: self.c }
    }
}

#[derive(Debug)]
struct Sample {
    before: Machine,
    after: Machine,
    instruction: UnknownInstruction,
}

fn read_samples(lines: Vec<String>) -> Vec<Sample> {
    lines.chunks(3).map (|chunk| {
        let before:&str = &chunk[0]["Before: ".len()..];
        let after:&str = &chunk[2]["After:  ".len()..];

        Sample { before: Machine::from_line(before), after: Machine::from_line(after), instruction: UnknownInstruction::from_line(&chunk[1]) }
    }).collect()
}

fn get_possible_opcodes(sample: &Sample) -> HashSet<&'static str> {
    let mut possible:HashSet<&str> = HashSet::new();

    for opcode in ALL_KNOWN_OPCODES.iter() {
        let instruction = sample.instruction.set_opcode(opcode);
        let mut machine = sample.before.clone();

        machine.execute(&instruction);

        if machine == sample.after {
            possible.insert(opcode);
        }
    }
    possible
}

fn construct_possible_mapping(samples: &Vec<Sample>) -> HashMap<u8, HashSet<&str>> {
    let mut mapping:HashMap<u8, HashSet<&str>> = HashMap::new();

    for sample in samples.iter() {
        let possible_opcodes = get_possible_opcodes(sample);

        mapping.entry(sample.instruction.opcode)
            .and_modify(|set| {
                set.retain(|item| possible_opcodes.contains(item));
            })
            .or_insert(possible_opcodes);
    }
    mapping
}

fn reduce_possible_mapping(mapping: HashMap<u8, HashSet<&str>>) -> HashMap<u8, &str> {
    let mut mapping = mapping;
    let mut known:HashMap<u8, &str> = HashMap::new();

    let mut queue:VecDeque<u8> = VecDeque::new();
    for (opcode, explanations) in mapping.iter() {
        if 1 == explanations.len() {
            queue.push_back(*opcode)
        }
    }

    while ! queue.is_empty() {
        let opcode = queue.pop_front().unwrap();
        let explanations = mapping.remove(&opcode).unwrap();
        let name = explanations.into_iter().collect::<Vec<&str>>()[0];
        known.insert(opcode, name);

        for (opcode, explanations) in mapping.iter_mut() {
            if explanations.contains(name) {
                explanations.remove(name);
                if 1 == explanations.len() {
                    queue.push_back(*opcode)
                }
            }
        }
    }

    known
}

fn read_instructions(lines: Vec<String>) -> Vec<UnknownInstruction> {
    lines
        .iter()
        .map(|line| UnknownInstruction::from_line(&line))
        .collect()
}

fn find_split_point(lines: &Vec<String>) -> Option<usize> {
    let mut n_empty_lines = 0;

    for (i, line) in lines.iter().enumerate() {
        if line.is_empty() && (2 == n_empty_lines) {
            return Some(i)
        } else if line.is_empty() {
            n_empty_lines += 1
        } else {
            n_empty_lines = 0
        }
    }

    None
}

fn part_1(samples: &Vec<Sample>) -> usize {
    let mut count = 0;

    for sample in samples {
        if 3 <= get_possible_opcodes(&sample).len() {
            count += 1
        }
    }

    count
}

fn part_2(samples: &Vec<Sample>, instructions: &Vec<UnknownInstruction>) -> u32 {
    let mapping = reduce_possible_mapping(construct_possible_mapping(samples));

    let mut machine = Machine::new();
    for instruction in instructions {
        let opcode_name = mapping.get(&instruction.opcode).unwrap();
        machine.execute(&instruction.set_opcode(opcode_name));
    }

    machine.registers[0]
}

pub fn solve(lines: Vec<String>) {
    let mut lines = lines;
    let mut code_lines = lines.split_off(find_split_point(&lines).unwrap());

    lines.retain(|line| !line.is_empty());
    code_lines.retain(|line| !line.is_empty());

    let samples = read_samples(lines);
    let instructions = read_instructions(code_lines);

    println!("Day 16");
    println!("Part 1: {}", part_1(&samples));
    println!("Part 2: {}", part_2(&samples, &instructions));
}
