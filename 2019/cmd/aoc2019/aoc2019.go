package main

import (
	"github.com/kdeberk/advent-of-code/2019/internal/day1"
 	"github.com/kdeberk/advent-of-code/2019/internal/day10"
	"github.com/kdeberk/advent-of-code/2019/internal/day11"
	"github.com/kdeberk/advent-of-code/2019/internal/day12"
	"github.com/kdeberk/advent-of-code/2019/internal/day13"
	"github.com/kdeberk/advent-of-code/2019/internal/day14"
	"github.com/kdeberk/advent-of-code/2019/internal/day15"
	"github.com/kdeberk/advent-of-code/2019/internal/day16"
	"github.com/kdeberk/advent-of-code/2019/internal/day17"
	"github.com/kdeberk/advent-of-code/2019/internal/day18"
	"github.com/kdeberk/advent-of-code/2019/internal/day19"
	"github.com/kdeberk/advent-of-code/2019/internal/day2"
	"github.com/kdeberk/advent-of-code/2019/internal/day20"
	"github.com/kdeberk/advent-of-code/2019/internal/day21"
	"github.com/kdeberk/advent-of-code/2019/internal/day22"
	"github.com/kdeberk/advent-of-code/2019/internal/day23"
	"github.com/kdeberk/advent-of-code/2019/internal/day24"
	"github.com/kdeberk/advent-of-code/2019/internal/day25"
	"github.com/kdeberk/advent-of-code/2019/internal/day3"
	"github.com/kdeberk/advent-of-code/2019/internal/day4"
	"github.com/kdeberk/advent-of-code/2019/internal/day5"
	"github.com/kdeberk/advent-of-code/2019/internal/day6"
	"github.com/kdeberk/advent-of-code/2019/internal/day7"
	"github.com/kdeberk/advent-of-code/2019/internal/day8"
	"github.com/kdeberk/advent-of-code/2019/internal/day9"
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
