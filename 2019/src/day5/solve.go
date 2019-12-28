package day5

import (
	"fmt"
	"utils"
)

func runDiagnostics(machine utils.Machine, systemId int64) (int64, error) {
	go machine.Run()

	var outputs []int64

	for {
		select {
		case machine.Input <- systemId:
		case err := <-machine.Error:
			if err != nil {
				return 0, err
			} else {
				return outputs[len(outputs)-1], nil
			}
		case output := <-machine.Output:
			outputs = append(outputs, output)
		}
	}
}

func part1(machine utils.Machine) (int64, error) {
	return runDiagnostics(machine, 1)
}

func part2(machine utils.Machine) (int64, error) {
	return runDiagnostics(machine, 5)
}

func Solve() error {
	program, err := utils.ReadProgram("day5/5.txt")
	if err != nil {
		return err
	}

	machine := utils.MakeMachine("day5", program)

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
