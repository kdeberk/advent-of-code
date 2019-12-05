package main

import (
	"fmt"
	"utils"
)

const part1Answer = 13978427

type Machine struct {
	ip      uint64
	memory  [1000]int64
	halted  bool
	inputs  []int64
	outputs []int64
}

const (
	Add         int64 = 1
	Multiply    int64 = 2
	Input       int64 = 3
	Output      int64 = 4
	JumpIfTrue  int64 = 5
	JumpIfFalse int64 = 6
	LessThan    int64 = 7
	Equals      int64 = 8
	Halt        int64 = 99
)

const (
	PositionalMode   int64 = 0
	IntermediateMode int64 = 1
)

type Operand struct {
	mode  int64
	value int64
}

type Instruction struct {
	opcode int64
	left   Operand
	right  Operand
	target int64
	size   uint64
}

func (self Operand) getValue(machine *Machine) int64 {
	switch self.mode {
	case PositionalMode:
		return machine.memory[self.value]
	case IntermediateMode:
		return self.value
	default:
		panic("Unknown mode")
	}
}

func (self *Machine) execute(instruction Instruction) {
	jumped := false

	switch instruction.opcode {
	case Add:
		self.memory[instruction.target] = instruction.left.getValue(self) + instruction.right.getValue(self)
	case Multiply:
		self.memory[instruction.target] = instruction.left.getValue(self) * instruction.right.getValue(self)
	case Input:
		var input int64
		input, self.inputs = self.inputs[0], self.inputs[1:]
		self.memory[instruction.left.value] = input
	case Output:
		self.outputs = append(self.outputs, instruction.left.getValue(self))
	case JumpIfTrue:
		if 0 != instruction.left.getValue(self) {
			self.ip = uint64(instruction.right.getValue(self))
			jumped = true
		}
	case JumpIfFalse:
		if 0 == instruction.left.getValue(self) {
			self.ip = uint64(instruction.right.getValue(self))
			jumped = true
		}
	case LessThan:
		if instruction.left.getValue(self) < instruction.right.getValue(self) {
			self.memory[instruction.target] = 1
		} else {
			self.memory[instruction.target] = 0
		}
	case Equals:
		if instruction.left.getValue(self) == instruction.right.getValue(self) {
			self.memory[instruction.target] = 1
		} else {
			self.memory[instruction.target] = 0
		}
	case Halt:
		self.halted = true
	}

	if !jumped {
		self.ip += instruction.size
	}
}

func (self *Machine) nextInstruction() (Instruction, error) {
	var opcode, target int64
	var left, right Operand
	var size uint64

	opcode = self.memory[self.ip] % 100
	switch opcode {
	case Add, Multiply, LessThan, Equals:
		left.value = self.memory[self.ip+1]
		left.mode = (self.memory[self.ip] / 100) % 10
		right.value = self.memory[self.ip+2]
		right.mode = (self.memory[self.ip] / 1000) % 10
		target = self.memory[self.ip+3]
		size = 4
	case JumpIfTrue, JumpIfFalse:
		left.value = self.memory[self.ip+1]
		left.mode = (self.memory[self.ip] / 100) % 10
		right.value = self.memory[self.ip+2]
		right.mode = (self.memory[self.ip] / 1000) % 10
		size = 3
	case Input, Output:
		left.value = self.memory[self.ip+1]
		left.mode = (self.memory[self.ip] / 100) % 10
		size = 2
	case Halt:
		size = 1
	default:
		return Instruction{}, fmt.Errorf("Unknown opcode %d", opcode)
	}

	return Instruction{opcode, left, right, target, size}, nil
}

func (self *Machine) loadProgram(filename string) error {
	ints, err := utils.ReadInt64s(filename, utils.IsWhiteSpaceOrComma)
	if err != nil {
		return err
	}

	for index, int := range ints {
		self.memory[index] = int
	}
	return nil
}

func (self *Machine) run() error {
	for !self.halted {
		next, err := self.nextInstruction()
		if err != nil {
			return err
		}

		self.execute(next)
	}
	return nil
}

func part1(machine Machine) (int64, error) {
	machine.inputs = append(machine.inputs, 1)
	err := machine.run()
	if err != nil {
		return 0, err
	}
	return machine.outputs[len(machine.outputs)-1], nil
}

func part2(machine Machine) (int64, error) {
	machine.inputs = append(machine.inputs, 5)
	err := machine.run()
	if err != nil {
		return 0, err
	}
	return machine.outputs[len(machine.outputs)-1], nil
}

func main() {
	machine := Machine{}
	machine.loadProgram("../data/5.txt")

	var answer int64
	answer, err := part1(machine)
	if err != nil {
		fmt.Println(err)
	} else if part1Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part1Answer))
	}
	fmt.Printf("Part 1: %d\n", answer)

	answer, err = part2(machine)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Printf("Part 2: %d\n", answer)
}
