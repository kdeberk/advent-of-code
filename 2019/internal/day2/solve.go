package day2

import (
	"fmt"
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

func runMachine(machine utils.Machine, noun, verb int64) (int64, error) {
	machine.SetMemory(1, noun)
	machine.SetMemory(2, verb)

	go machine.Run()
	err := machine.WaitForTermination()
	if err != nil {
		return 0, err
	}
	return machine.GetMemory(0), nil
}

func part1(machine utils.Machine) (int64, error) {
	return runMachine(machine, 12, 2)
}

func part2(machine utils.Machine) (int64, error) {
	var noun, verb int64

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
	program, err := utils.ReadProgram("./input/2.txt")
	if err != nil {
		return err
	}

	machine := utils.MakeMachine("day2", program)

	var answer int64
	answer, err = part1(machine)
	if nil != err {
		return err
	}
	fmt.Println("Day 2. Part 1. Construct a working Intcode computer and restore the gravity assist program.")
	fmt.Println(" ", answer)

	answer, err = part2(machine)
	if nil != err {
		return err
	}
	fmt.Println("Day 2. Part 2. Find the input parameters that generate a certain output value.")
	fmt.Println(" ", answer)
	return nil
}
