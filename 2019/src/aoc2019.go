package main

import (
	"day1"
	"day10"
	"day11"
	"day12"
	"day13"
	"day14"
	"day15"
	"day16"
	"day17"
	"day18"
	"day19"
	"day2"
	"day20"
	"day21"
	"day22"
	"day23"
	"day24"
	"day25"
	"day3"
	"day4"
	"day5"
	"day6"
	"day7"
	"day8"
	"day9"
	"flag"
	"fmt"
	"os"
)

type Solver func() error

var solvers = []Solver{
	day1.Solve, day2.Solve, day3.Solve, day4.Solve, day5.Solve, day6.Solve, day7.Solve,
	day8.Solve, day9.Solve, day10.Solve, day11.Solve, day12.Solve, day13.Solve, day14.Solve,
	day15.Solve, day16.Solve, day17.Solve, day18.Solve, day19.Solve, day20.Solve, day21.Solve,
	day22.Solve, day23.Solve, day24.Solve, day25.Solve,
}

func main() {
	dayPtr := flag.Int("day", 0, "Day to run solver for")
	flag.Parse()

	if isFlagSet("day") {
		if *dayPtr <= 0 || len(solvers) < *dayPtr {
			exitForError(fmt.Errorf("No solver found for day %d", *dayPtr))
		}
		runSolver(solvers[*dayPtr-1])
	} else {
		for _, solver := range solvers {
			runSolver(solver)
		}
	}
}

func runSolver(solver Solver) {
	err := solver()
	if err != nil {
		exitForError(err)
	}
}

func exitForError(err error) {
	fmt.Fprintf(os.Stderr, "error: %v\n", err)
	os.Exit(1)
}

func isFlagSet(name string) bool {
	found := false
	flag.Visit(func(flag *flag.Flag) {
		if name == flag.Name {
			found = true
		}
	})
	return found
}
