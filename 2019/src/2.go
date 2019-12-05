package main

import (
	"fmt"
	"utils"
)

const part1Answer = 9581917
const part2Answer = 2505

func runMachine(machine utils.Machine, noun int64, verb int64) (int64, error) {
	machine.SetMemory(1, noun)
	machine.SetMemory(2, verb)

	_, err := machine.Run([]int64{})
	if err != nil {
		return 0, err
	} else {
		return machine.GetMemory(0), nil
	}
}

func part1(machine utils.Machine) (int64, error) {
	return runMachine(machine, 12, 2)
}

func part2(machine utils.Machine) (int64, error) {
	var noun int64
	var verb int64
SearchLoop:
	for {
		for verb <= noun {
			result, err := runMachine(machine, noun, verb)
			if err != nil {
				return 0, err
			} else if result == 19690720 {
				break SearchLoop
			}
			verb += 1
		}
		noun += 1
		verb = 0
	}

	return 100*noun + verb, nil
}

func main() {
	program, err := utils.ReadProgram("../data/2.txt")
	if err != nil {
		panic(err)
	}

	machine := utils.Machine{}
	machine.LoadProgram(program)

	var answer int64
	answer, err = part1(machine)
	if part1Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part1Answer))
	}
	fmt.Printf("Part 1: %d\n", answer)

	answer, err = part2(machine)
	if part2Answer != answer {
		panic(fmt.Sprintf("Part 2 has wrong answer %d (correct %d)", answer, part2Answer))
	}
	fmt.Printf("Part 2: %d\n", answer)
}
