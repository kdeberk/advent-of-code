package day5

import (
	"fmt"
	"utils"
)

func runDiagnostics(machine *utils.Machine, systemId int64) (int64, error) {
	go machine.Run()

	var last_output int64
WaitForTerminationLoop:
	for {
		select {
		case machine.Input <- systemId:
		case err := <-machine.Error:
			if err != nil {
				return 0, err
			} else {
				break WaitForTerminationLoop
			}
		case last_output = <-machine.Output:
		}
	}
	return last_output, nil
}

func part1(machine utils.Machine) (int64, error) {
	return runDiagnostics(&machine, 1)
}

func part2(machine utils.Machine) (int64, error) {
	return runDiagnostics(&machine, 5)
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
	fmt.Println("Day 5, Part 1. Run a diagnostic program on System 1 of the Thermal Environment Supervisor Terminal (Test) machine.")
	fmt.Println(" ", answer)

	answer, err = part2(machine)
	if err != nil {
		return err
	}
	fmt.Println("Day 5, Part 2. Run a diagnostic program on System 5 of the Thermal Environment Supervisor Terminal (Test) machine.")
	fmt.Println(" ", answer)
	return nil
}
