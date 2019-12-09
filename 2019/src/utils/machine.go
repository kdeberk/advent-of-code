package utils

import (
	"fmt"
	"math"
)

// Welcome to the machine

type Machine struct {
	Name   string
	ip     uint64
	rbp    uint64
	memory [100000]int64
	halted bool
	Input  chan int64
	Output chan int64
	Error  chan error
}

func MakeMachine(name string) Machine {
	return Machine{
		name,
		0,
		0,
		[100000]int64{},
		false,
		make(chan int64),
		make(chan int64),
		make(chan error),
	}
}

const (
	add         int64 = 1
	multiply    int64 = 2
	input       int64 = 3
	output      int64 = 4
	jumpIfTrue  int64 = 5
	jumpIfFalse int64 = 6
	lessThan    int64 = 7
	equals      int64 = 8
	rbpOffset   int64 = 9
	halt        int64 = 99
)

type ParameterMode int64

const (
	PositionalMode   ParameterMode = 0
	IntermediateMode ParameterMode = 1
	RelativeMode     ParameterMode = 2
)

type Parameter struct {
	mode  ParameterMode
	value int64
}

type Instruction struct {
	opcode int64
	// TODO: rename parameters to first, second
	first  Parameter
	second Parameter
	third  Parameter
	size   uint64
}

func (self Parameter) getValue(machine *Machine) int64 {
	switch self.mode {
	case PositionalMode:
		return machine.memory[self.value]
	case IntermediateMode:
		return self.value
	case RelativeMode:
		return machine.memory[int64(machine.rbp)+self.value]
	default:
		panic("No such mode")
	}
}

func (self Parameter) getAddress(machine *Machine) int64 {
	switch self.mode {
	case PositionalMode:
		return self.value
	case IntermediateMode:
		panic("No such mode")
	case RelativeMode:
		return int64(machine.rbp) + self.value
	default:
		panic("No such mode")
	}
}

func (self *Machine) execute(instruction Instruction) error {
	jumped := false

	switch instruction.opcode {
	case add:
		self.memory[instruction.third.getAddress(self)] = instruction.first.getValue(self) + instruction.second.getValue(self)
	case multiply:
		self.memory[instruction.third.getAddress(self)] = instruction.first.getValue(self) * instruction.second.getValue(self)
	case input:
		input := <-self.Input
		self.memory[instruction.first.getAddress(self)] = input
	case output:
		self.Output <- instruction.first.getValue(self)
	case jumpIfTrue:
		if 0 != instruction.first.getValue(self) {
			self.ip = uint64(instruction.second.getValue(self))
			jumped = true
		}
	case jumpIfFalse:
		if 0 == instruction.first.getValue(self) {
			self.ip = uint64(instruction.second.getValue(self))
			jumped = true
		}
	case lessThan:
		if instruction.first.getValue(self) < instruction.second.getValue(self) {
			self.memory[instruction.third.getAddress(self)] = 1
		} else {
			self.memory[instruction.third.getAddress(self)] = 0
		}
	case equals:
		if instruction.first.getValue(self) == instruction.second.getValue(self) {
			self.memory[instruction.third.getAddress(self)] = 1
		} else {
			self.memory[instruction.third.getAddress(self)] = 0
		}
	case rbpOffset:
		self.rbp = uint64(int64(self.rbp) + instruction.first.getValue(self))
	case halt:
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
	case PositionalMode, IntermediateMode, RelativeMode:
		break
	default:
		return Parameter{}, fmt.Errorf("Not a valid parameter mode %d", mode)
	}

	return Parameter{ParameterMode(mode), self.memory[self.ip+uint64(nth)]}, nil
}

func (self *Machine) nextInstruction() (Instruction, error) {
	var opcode int64
	var first, second, third Parameter
	var size uint64
	var err error

	opcode = self.memory[self.ip] % 100
	switch opcode {
	case add, multiply, lessThan, equals:
		first, err = self.readParameter(1)
		if err != nil {
			return Instruction{}, err
		}
		second, err = self.readParameter(2)
		if err != nil {
			return Instruction{}, err
		}
		third, err = self.readParameter(3)
		if err != nil {
			return Instruction{}, err
		}
		size = 4
	case jumpIfTrue, jumpIfFalse:
		first, err = self.readParameter(1)
		if err != nil {
			return Instruction{}, err
		}
		second, err = self.readParameter(2)
		if err != nil {
			return Instruction{}, err
		}
		size = 3
	case input, output, rbpOffset:
		first, err = self.readParameter(1)
		if err != nil {
			return Instruction{}, err
		}
		size = 2
	case halt:
		size = 1
	default:
		return Instruction{}, fmt.Errorf("Unknown opcode %d", opcode)
	}

	return Instruction{opcode, first, second, third, size}, nil
}

type Program []int64

func (self *Machine) LoadProgram(program Program) {
	for index, int := range program {
		self.memory[index] = int
	}
}

func (self *Machine) Run() {
	for !self.halted {
		next, err := self.nextInstruction()
		if err != nil {
			self.Error <- err
			return
		}

		err = self.execute(next)
		if err != nil {
			self.Error <- err
			return
		}
	}
	self.Error <- nil
}

func (self *Machine) SetMemory(index uint64, value int64) {
	self.memory[index] = value
}

func (self *Machine) GetMemory(index uint64) int64 {
	return self.memory[index]
}

func ReadProgram(filename string) (Program, error) {
	return ReadInt64s(filename, IsWhiteSpaceOrComma)
}
