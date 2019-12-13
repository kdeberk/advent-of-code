package main

import (
	"day1"
	"day10"
	"day11"
	"day12"
	"day13"
	"day2"
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
	day8.Solve, day9.Solve, day10.Solve, day11.Solve, day12.Solve, day13.Solve,
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
