package main

import (
	"day1"
	"day2"
	"day3"
	"day4"
	"day5"
	"day6"
	"flag"
	"fmt"
	"os"
)

var solvers = []func() error{
	day1.Solve, day2.Solve, day3.Solve, day4.Solve, day5.Solve, day6.Solve,
}

func main() {
	dayPtr := flag.Int("day", 0, "Day to run solver for")
	flag.Parse()

	if isFlagSet("day") {
		if *dayPtr <= 0 || len(solvers) < *dayPtr {
			exitForError(fmt.Errorf("No solver found for day %d", *dayPtr))
		}
		solvers[*dayPtr-1]()
	} else {
		for _, solver := range solvers {
			solver()
		}
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
