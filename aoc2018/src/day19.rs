const IP_REGISTER:usize = 3;
const N_REGISTERS:usize = 6;

struct Machine {
    ip: u32,
    registers: [u32; N_REGISTERS],
}

impl Machine {
    fn new() -> Self {
        Self { ip: 0, registers: [0u32; N_REGISTERS] }
    }

    fn execute(&mut self, instruction: &Instruction) {
        let a = instruction.a;
        let b = instruction.b;
        let ra = instruction.a as usize;
        let rb = instruction.b as usize;
        let rc = instruction.c as usize;

        self.registers[IP_REGISTER] = self.ip;
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
        self.ip = self.registers[IP_REGISTER] + 1;
    }
}

#[derive(Debug)]
struct Instruction {
    opcode: String,
    a: u32,
    b: u32,
    c: u32
}

impl Instruction {
    fn from_line(line: &str) -> Self {
        let operands:Vec<&str> = line.split(" ").collect();

        Self { opcode: String::from(operands[0]), a: operands[1].parse().unwrap(), b: operands[2].parse().unwrap(), c: operands[3].parse().unwrap() }
    }
}

fn read_instructions(lines: Vec<String>) -> Vec<Instruction> {
    lines
        .iter()
        .filter(|line| !line.starts_with("#"))
        .map(|line| Instruction::from_line(line))
        .collect()
}

fn part_1(instructions: &Vec<Instruction>) -> u32 {
    let mut machine = Machine::new();

    loop {
        let ip = machine.ip as usize;

        if let Some(instruction) = instructions.get(ip) {
            machine.execute(instruction);
        } else {
            break
        }
    }
    machine.registers[0]
}

pub fn solve(lines: Vec<String>) {
    let instructions = read_instructions(lines);

    println!("Day 19");
    println!("Part 1: {}", part_1(&instructions));
    println!("Part 2: __");
}
