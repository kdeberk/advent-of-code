use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

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
            "gtir" => {
                if a > self.registers[rb] {
                    self.registers[rc] = 1
                } else {
                    self.registers[rc] = 0
                }
            },
            "gtri" => {
                if self.registers[ra] > b {
                    self.registers[rc] = 1
                } else {
                    self.registers[rc] = 0
                }
            },
            "gtrr" => {
                if self.registers[ra] > self.registers[rb] {
                    self.registers[rc] = 1
                } else {
                    self.registers[rc] = 0
                }
            },
            "eqir" => {
                if a == self.registers[rb] {
                    self.registers[rc] = 1
                } else {
                    self.registers[rc] = 0
                }
            },
            "eqri" => {
                if self.registers[ra] == b {
                    self.registers[rc] = 1
                } else {
                    self.registers[rc] = 0
                }
            },
            "eqrr" => {
                if self.registers[ra] == self.registers[rb] {
                    self.registers[rc] = 1
                } else {
                    self.registers[rc] = 0
                }
            },
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

fn read_samples(filename: &str) -> Vec<Sample> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(&file);
    let lines:Vec<String> = reader
        .lines()
        .map(|line| line.unwrap())
        .filter(|line| 0 < line.len())
        .collect();

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

fn read_instructions(filename: &str) -> Vec<UnknownInstruction> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(&file);

    reader.lines().map(|line| {
        UnknownInstruction::from_line(&line.unwrap())
    }).collect()
}

fn part_1(samples_filename: &str) {
    let samples = read_samples(samples_filename);
    let mut count = 0;

    for sample in samples {
        if 3 <= get_possible_opcodes(&sample).len() {
            count += 1
        }
    }

    println!("{:?}", count);
}

fn part_2(samples_filename: &str, code_filename: &str) {
    let samples = read_samples(samples_filename);
    let mapping = reduce_possible_mapping(construct_possible_mapping(&samples));
    let instructions = read_instructions(code_filename);

    let mut machine = Machine::new();
    for instruction in instructions {
        let opcode_name = mapping.get(&instruction.opcode).unwrap();
        machine.execute(&instruction.set_opcode(opcode_name));
    }

    println!("{}", machine.registers[0])
}

fn main() {
    part_1("samples");
    part_2("samples", "code");
}
