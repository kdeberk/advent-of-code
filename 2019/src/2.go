package main

import (
	"fmt"
	"line_reader"
	"strconv"
	"strings"
)

type Machine [1000]uint64

const (
	Add      uint64 = 1
	Multiply uint64 = 2
	Halt     uint64 = 99
)

func loadMachine(filename string) (Machine, error) {
	reader, err := line_reader.NewLineReader(filename)
	if err != nil {
		return Machine{}, err
	}

	line := reader.ReadLine()
	numbers := strings.Split(line, ",")
	machine := Machine{}
	for index, i := range numbers {
		i, err := strconv.ParseUint(i, 10, 64)
		if err != nil {
			return Machine{}, err
		}
		machine[index] = i
	}

	return machine, nil
}

func runMachine(start Machine, noun uint64, verb uint64) uint64 {
	machine := start

	machine[1] = noun
	machine[2] = verb

	var index int
RunLoop:
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
			break RunLoop
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

	answer1 := part1(machine)
	fmt.Printf("Part 1: %d\n", answer1)
	answer2 := part2(machine)
	fmt.Printf("Part 2: %d\n", answer2)
}
