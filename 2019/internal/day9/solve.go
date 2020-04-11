package day9

import (
	"fmt"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

func runMachine(program utils.Program, mode int64) (int64, error) {
	machine := utils.MakeMachine("day9", program)
	go machine.Run()

	var lastOutput int64
RunLoop:
	for {
		select {
		case machine.Input <- mode:
		case output := <-machine.Output:
			lastOutput = output
		case err := <-machine.Error:
			if err != nil {
				return 0, err
			} else {
				break RunLoop
			}
		}
	}

	return lastOutput, nil
}

func part1(program utils.Program) (int64, error) {
	return runMachine(program, 1)
}

func part2(program utils.Program) (int64, error) {
	return runMachine(program, 2)
}

func Solve() error {
	program, err := utils.ReadProgram("./input/9.txt")
	if err != nil {
		return err
	}

	var answer int64
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 9, Part 1. Received a distress signal, but in order to lock on we need to boost the sensors.")
	fmt.Println(" ", answer)

	answer, err = part2(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 9, Part 2. Coordinates of the distress signal that was received from Ceres.")
	fmt.Println(" ", answer)
	return nil
}
