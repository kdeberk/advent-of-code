package main

import (
	"fmt"
	"utils"
)

const part1Answer = 13978427
const part2Answer = 11189491

func part1(machine utils.Machine) (int64, error) {
	outputs, err := machine.Run([]int64{1})
	if err != nil {
		return 0, err
	}
	return outputs[len(outputs)-1], nil
}

func part2(machine utils.Machine) (int64, error) {
	outputs, err := machine.Run([]int64{5})
	if err != nil {
		return 0, err
	}
	return outputs[len(outputs)-1], nil
}

func main() {
	program, err := utils.ReadProgram("../data/5.txt")
	if err != nil {
		panic(err)
	}

	machine := utils.Machine{}
	machine.LoadProgram(program)

	var answer int64
	answer, err = part1(machine)
	if err != nil {
		fmt.Println(err)
	} else if part1Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part1Answer))
	}
	fmt.Printf("Part 1: %d\n", answer)

	answer, err = part2(machine)
	if err != nil {
		fmt.Println(err)
	} else if part2Answer != answer {
		panic(fmt.Sprintf("Part 2 has wrong answer %d (correct %d)", answer, part2Answer))
	}
	fmt.Printf("Part 2: %d\n", answer)
}
