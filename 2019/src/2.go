package main

import (
	"fmt"
	"utils"
)

const part1Answer = 9581917
const part2Answer = 2505

type Machine [1000]uint64

const (
	Add      uint64 = 1
	Multiply uint64 = 2
	Halt     uint64 = 99
)

func loadMachine(filename string) (Machine, error) {
	numbers, err := utils.ReadUint64s("../data/2.txt", utils.IsWhiteSpaceOrComma)
	if err != nil {
		return Machine{}, err
	}

	machine := Machine{}
	for index, number := range numbers {
		machine[index] = number
	}

	return machine, nil
}

func runMachine(start Machine, noun uint64, verb uint64) uint64 {
	machine := start

	machine[1] = noun
	machine[2] = verb

	var index int
Execution:
	for index <= len(machine) {
		opcode := machine[index]
		left := machine[machine[index+1]]
		right := machine[machine[index+2]]
		target := machine[index+3]

		switch opcode {
		case Add:
			machine[target] = left + right
		case Multiply:
			machine[target] = left * right
		case Halt:
			break Execution
		}

		index += 4
	}

	return machine[0]
}

func part1(machine Machine) uint64 {
	return runMachine(machine, 12, 2)
}

func part2(machine Machine) uint64 {
	var noun uint64
	var verb uint64
SearchLoop:
	for {
		for verb <= noun {
			result := runMachine(machine, noun, verb)

			if result == 19690720 {
				break SearchLoop
			}
			verb += 1
		}
		noun += 1
		verb = 0
	}

	return 100*noun + verb
}

func main() {
	machine, err := loadMachine("../data/2.txt")
	if err != nil {
		fmt.Println(err)
	}

	var answer uint64
	answer = part1(machine)
	if part1Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part1Answer))
	}
	fmt.Printf("Part 1: %d\n", answer)

	answer = part2(machine)
	if part2Answer != answer {
		panic(fmt.Sprintf("Part 2 has wrong answer %d (correct %d)", answer, part2Answer))
	}
	fmt.Printf("Part 2: %d\n", answer)
}
