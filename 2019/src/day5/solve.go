package day5

import (
	"fmt"
	"utils"
)

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

func Solve() error {
	program, err := utils.ReadProgram("day5/5.txt")
	if err != nil {
		return err
	}

	machine := utils.Machine{}
	machine.LoadProgram(program)

	var answer int64
	answer, err = part1(machine)
	if err != nil {
		return err
	}
	fmt.Printf("Day 5, Part 1: %d\n", answer)

	answer, err = part2(machine)
	if err != nil {
		return err
	}
	fmt.Printf("Day 5, Part 2: %d\n", answer)
	return nil
}
