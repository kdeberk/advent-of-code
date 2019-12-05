package utils

import (
	"fmt"
	"math"
)

// Welcome to the machine

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

type ParameterMode int64

const (
	PositionalMode   ParameterMode = 0
	IntermediateMode ParameterMode = 1
)

type Parameter struct {
	mode  ParameterMode
	value int64
}

type Instruction struct {
	opcode int64
	left   Parameter
	right  Parameter
	target int64
	size   uint64
}

func (self Parameter) getValue(machine *Machine) int64 {
	switch self.mode {
	case PositionalMode:
		return machine.memory[self.value]
	case IntermediateMode:
		return self.value
	default:
		panic("No such mode")
	}
}

func (self *Machine) execute(instruction Instruction) error {
	jumped := false

	switch instruction.opcode {
	case Add:
		self.memory[instruction.target] = instruction.left.getValue(self) + instruction.right.getValue(self)
	case Multiply:
		self.memory[instruction.target] = instruction.left.getValue(self) * instruction.right.getValue(self)
	case Input:
		if 0 == len(self.inputs) {
			return fmt.Errorf("No more inputs available")
		}

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
	return nil
}

func (self *Machine) readParameter(nth int64) (Parameter, error) {
	mode := ParameterMode((self.memory[self.ip] / int64(math.Pow(10, float64(1+nth)))) % 10)
	switch mode {
	case PositionalMode, IntermediateMode:
		break
	default:
		return Parameter{}, fmt.Errorf("Not a valid parameter mode %d", mode)
	}

	return Parameter{ParameterMode(mode), self.memory[self.ip+uint64(nth)]}, nil
}

func (self *Machine) nextInstruction() (Instruction, error) {
	var opcode, target int64
	var left, right Parameter
	var size uint64
	var err error

	opcode = self.memory[self.ip] % 100
	switch opcode {
	case Add, Multiply, LessThan, Equals:
		left, err = self.readParameter(1)
		if err != nil {
			return Instruction{}, err
		}
		right, err = self.readParameter(2)
		if err != nil {
			return Instruction{}, err
		}
		target = self.memory[self.ip+3]
		size = 4
	case JumpIfTrue, JumpIfFalse:
		left, err = self.readParameter(1)
		if err != nil {
			return Instruction{}, err
		}
		right, err = self.readParameter(2)
		if err != nil {
			return Instruction{}, err
		}
		size = 3
	case Input, Output:
		left, err = self.readParameter(1)
		if err != nil {
			return Instruction{}, err
		}
		size = 2
	case Halt:
		size = 1
	default:
		return Instruction{}, fmt.Errorf("Unknown opcode %d", opcode)
	}

	return Instruction{opcode, left, right, target, size}, nil
}

func (self *Machine) LoadProgram(program []int64) {
	for index, int := range program {
		self.memory[index] = int
	}
}

func (self *Machine) Run(inputs []int64) ([]int64, error) {
	self.inputs = inputs

	for !self.halted {
		next, err := self.nextInstruction()
		if err != nil {
			return []int64{}, err
		}

		err = self.execute(next)
		if err != nil {
			return []int64{}, err
		}
	}

	return self.outputs, nil
}

func (self *Machine) SetMemory(index uint64, value int64) {
	self.memory[index] = value
}

func (self *Machine) GetMemory(index uint64) int64 {
	return self.memory[index]
}

func ReadProgram(filename string) ([]int64, error) {
	return ReadInt64s(filename, IsWhiteSpaceOrComma)
}
