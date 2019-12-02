package main

import (
	"fmt"
	"line_reader"
	"strconv"
)

func calculateFuel(weight uint64) uint64 {
	result := int64(weight)/3 - 2
	if result <= 0 {
		return 0
	} else {
		return uint64(result)
	}
}

func part1(filename string) (uint64, error) {
	reader, err := line_reader.NewLineReader(filename)
	if err != nil {
		return 0, err
	}

	var fuelsum uint64
	for reader.HasLine() {
		line := reader.ReadLine()
		weight, err := strconv.ParseUint(line, 10, 64)
		if err != nil {
			fmt.Println(line + " could not be converted to an integer")
			return 0, err
		}
		fuelsum += calculateFuel(weight)
	}
	return fuelsum, nil
}

func part2(filename string) (uint64, error) {
	reader, err := line_reader.NewLineReader(filename)
	if err != nil {
		return 0, err
	}

	var fuelsum uint64
	for reader.HasLine() {
		line := reader.ReadLine()
		weight, err := strconv.ParseUint(line, 10, 64)
		if err != nil {
			fmt.Println(line + " could not be converted to an integer")
			return 0, err
		}

		for 0 < weight {
			fuel := calculateFuel(weight)
			fuelsum += fuel
			weight = fuel
		}
	}
	return fuelsum, nil
}

func main() {
	value1, err := part1("../data/1.txt")
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	fmt.Printf("Part 1: %d\n", value1)

	value2, err := part2("../data/1.txt")
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	fmt.Printf("Part 2: %d\n", value2)
}
