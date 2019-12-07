package day2

import (
	"fmt"
	"utils"
)

func runMachine(machine utils.Machine, noun int64, verb int64) (int64, error) {
	machine.SetMemory(1, noun)
	machine.SetMemory(2, verb)

	go machine.Run()
	err := <-machine.Error
	if err != nil {
		return 0, err
	}
	return machine.GetMemory(0), nil
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

func Solve() error {
	var answer int64

	program, err := utils.ReadProgram("day2/2.txt")
	if err != nil {
		return err
	}

	machine := utils.MakeMachine()
	machine.LoadProgram(program)

	answer, err = part1(machine)
	if nil != err {
		return err
	}
	fmt.Printf("Day 2, Part 1: %d\n", answer)

	answer, err = part2(machine)
	if nil != err {
		return err
	}
	fmt.Printf("Day 2, Part 2: %d\n", answer)
	return nil
}
