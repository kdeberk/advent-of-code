package day21

import (
	"fmt"
	"strings"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

func runSpringDroid(program utils.Program, formula string) (int, error) {
	droid := utils.MakeAsciiMachine("day21", program)
	go droid.Run()

	_, err := droid.ReceiveUntil(":\n")
	if err != nil {
		return 0, err
	}
	err = droid.SendString(formula)
	if err != nil {
		return 0, err
	}

	var prev int64
	builder := strings.Builder{}
RenderLoop:
	for {
		select {
		case output := <-droid.Output:
			builder.WriteRune(rune(output))
			if '\n' == output && '\n' == rune(prev) {
				builder.Reset()
			}
			prev = output
		case err := <-droid.Error:
			if err != nil {
				return 0, err
			}
			break RenderLoop
		}
	}

	return int(prev), nil
}

func part1(program utils.Program) (int, error) {
	// (!A OR !B OR !C) AND D
	return runSpringDroid(program, "NOT A T\nOR T J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n")
}

func part2(program utils.Program) (int, error) {
	// (!A OR !B OR !C) AND D AND (E || H)
	return runSpringDroid(program, "NOT A T\nOR T J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nNOT T T\nOR E T\nOR H T\nAND T J\nRUN\n")
}

func Solve() error {
	program, err := utils.ReadProgram("./input/21.txt")
	if err != nil {
		return err
	}

	var answer int
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Printf("Day 21, Part 1: %d\n", answer)

	answer, err = part2(program)
	if err != nil {
		return err
	}
	fmt.Printf("Day 21, Part 2: %d\n", answer)

	return nil
}
