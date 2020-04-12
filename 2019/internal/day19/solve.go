package day19

import (
	"fmt"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

func testCoordinate(program utils.Program, x, y int) bool {
	machine := utils.MakeMachine("day19", program)
	go machine.Run()

	machine.Input <- int64(x)
	machine.Input <- int64(y)
	output := <-machine.Output
	<-machine.Error
	return 1 == output
}

func part1(program utils.Program) int {
	count := 0

	for x := 0; x < 50; x += 1 {
		for y := 0; y < 50; y += 1 {
			if testCoordinate(program, x, y) {
				count += 1
			}
		}
	}

	return count
}

func part2(program utils.Program) int {
	var x, y int = 0, 6
SearchLoop:
	for ; ; y += 1 {
		for !testCoordinate(program, x, y) || testCoordinate(program, x+1, y) {
			x += 1
		}

		if x < 100 {
			continue SearchLoop
		}

		if testCoordinate(program, x-99, y) && testCoordinate(program, x-99, y+99) && testCoordinate(program, x, y+99) {
			break SearchLoop
		}
	}

	return (x-99)*10_000 + y
}

func Solve() error {
	program, err := utils.ReadProgram("./input/19.txt")
	if err != nil {
		return err
	}

	fmt.Println("Day 19, Part 1. Calculate the area of the tractor beam.")
	fmt.Println(" ", part1(program))
	fmt.Println("Day 19, Part 2. Determine the point at which the tractor beam can hold the spaceship.")
	fmt.Println(" ", part2(program))

	return nil
}
