package main

import (
	"fmt"
	"line_reader"
	"strconv"
	"strings"
)

func run_program(program []string, noun uint64, verb uint64) (uint64, error) {
	var instructions [1000]uint64

	for index, i := range program {
		i, err := strconv.ParseUint(i, 10, 64)
		if err != nil {
			return 0, err
		}
		instructions[index] = i
	}

	instructions[1] = noun
	instructions[2] = verb

	var index int
L:
	for index <= len(instructions) {
		opcode := instructions[index]
		left := instructions[instructions[index+1]]
		right := instructions[instructions[index+2]]
		target := instructions[index+3]

		switch opcode {
		case 1:
			instructions[target] = left + right
		case 2:
			instructions[target] = left * right
		case 99:
			break L
		}

		index += 4
	}

	return instructions[0], nil
}

func part1(filename string) (uint64, error) {
	reader, err := line_reader.NewLineReader(filename)
	if err != nil {
		return 0, err
	}

	line := reader.ReadLine()
	program := strings.Split(line, ",")

	return run_program(program, 12, 2)
}

func part2(filename string) (uint64, error) {
	reader, err := line_reader.NewLineReader(filename)
	if err != nil {
		return 0, err
	}

	line := reader.ReadLine()
	program := strings.Split(line, ",")

	var noun uint64
	var verb uint64
L:
	for {
		for verb <= noun {
			result, err := run_program(program, noun, verb)

			if err != nil {
				return 0, err
			} else if result == 19690720 {
				break L
			}
			verb += 1
		}
		noun += 1
		verb = 0
	}

	return 100*noun + verb, nil
}

func main() {
	value1, err := part1("../data/2.txt")
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	fmt.Printf("Part 1: %d\n", value1)

	value2, err := part2("../data/2.txt")
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	fmt.Printf("Part 2: %d\n", value2)
}
