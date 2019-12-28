package day9

import (
	"fmt"
	"utils"
)

func runMachine(program utils.Program, mode int64) (int64, error) {
	machine := utils.MakeMachine("day9", program)
	go machine.Run()

	var lastOutput int64
	for {
		select {
		case machine.Input <- mode:
		case output := <-machine.Output:
			lastOutput = output
		case err := <-machine.Error:
			if err != nil {
				return 0, err
			} else {
				return lastOutput, nil
			}
		}
	}
}

func part1(program utils.Program) (int64, error) {
	return runMachine(program, 1)
}

func part2(program utils.Program) (int64, error) {
	return runMachine(program, 2)
}

func Solve() error {
	program, err := utils.ReadProgram("day9/9.txt")
	if err != nil {
		return err
	}

	var answer int64
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Printf("Day 9, Part 1: %d\n", answer)

	answer, err = part2(program)
	if err != nil {
		return err
	}
	fmt.Printf("Day 9, Part 2: %d\n", answer)
	return nil
}
