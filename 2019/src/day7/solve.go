package day7

import (
	"fmt"
	"utils"
)

func makeAmplifiers(names []string, program utils.Program) []utils.Machine {
	amplifiers := []utils.Machine{}

	for i := 0; i < len(names); i++ {
		amplifier := utils.MakeMachine(names[i], program)
		amplifiers = append(amplifiers, amplifier)

		if 0 < i {
			amplifiers[i].Input = amplifiers[i-1].Output
			amplifiers[i].Error = amplifiers[0].Error
		}
	}

	return amplifiers
}

func runAmplifiers(amplifiers []utils.Machine, settings []int64, feedback bool) (int64, error) {
	first, last := amplifiers[0], amplifiers[len(amplifiers)-1]

	for i := 0; i < len(amplifiers); i++ {
		go amplifiers[i].Run()

		select {
		case amplifiers[i].Input <- settings[i]:
		case err := <-amplifiers[i].Error:
			return 0, fmt.Errorf("Amplifier %s unexpectedly terminated: %v", amplifiers[i].Name, err)
		}
	}

	first.Input <- 0

	var lastOutput int64
	nLivingAmplifiers := len(amplifiers)

	for 0 < nLivingAmplifiers {
		select {
		case lastOutput = <-last.Output:
		case err := <-amplifiers[0].Error:
			if err != nil {
				return 0, err
			} else {
				nLivingAmplifiers -= 1
			}
		}

		if feedback {
			select {
			case first.Input <- lastOutput:
			case err := <-amplifiers[0].Error:
				if err != nil {
					return 0, err
				} else {
					// N.B. It's okay if feedback wasn't sent, the circuit is no longer functional anyway.
					nLivingAmplifiers -= 1
				}
			}
		}
	}
	return lastOutput, nil
}

func part1(program utils.Program) (int64, error) {
	var max int64

	for perm := range utils.GeneratePermutations([]int64{0, 1, 2, 3, 4}) {
		output, err := runAmplifiers(makeAmplifiers([]string{"A", "B", "C", "D", "E"}, program), perm, false)
		if err != nil {
			return 0, err
		} else if max < output {
			max = output
		}
	}

	return max, nil
}

func part2(program utils.Program) (int64, error) {
	var max int64

	for perm := range utils.GeneratePermutations([]int64{5, 6, 7, 8, 9}) {
		output, err := runAmplifiers(makeAmplifiers([]string{"A", "B", "C", "D", "E"}, program), perm, true)
		if err != nil {
			return 0, err
		} else if max < output {
			max = output
		}
	}
	return max, nil
}

func Solve() error {
	program, err := utils.ReadProgram("day7/7.txt")
	if err != nil {
		return err
	}

	var answer int64
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 7, Part 1. Try every settings permutation on a series of Intcode amplifiers and find the highest output value.")
	fmt.Println(" ", answer)

	answer, err = part2(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 7, Part 2. Try every settings permutation on a series of Intcode amplifiers with a feedback loop, and find the highest output value.")
	fmt.Println(" ", answer)

	return nil
}
