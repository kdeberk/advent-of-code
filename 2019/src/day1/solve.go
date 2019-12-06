package day1

import (
	"fmt"
	"utils"
)

func calculateFuel(weight uint64) uint64 {
	result := int64(weight)/3 - 2
	if result <= 0 {
		return 0
	} else {
		return uint64(result)
	}
}

func part1(weights []uint64) uint64 {
	var fuelsum uint64
	for _, weight := range weights {
		fuelsum += calculateFuel(weight)
	}
	return fuelsum
}

func part2(weights []uint64) uint64 {
	var fuelsum uint64
	for _, weight := range weights {
		for 0 < weight {
			fuel := calculateFuel(weight)
			fuelsum += fuel
			weight = fuel
		}
	}
	return fuelsum
}

func Solve() error {
	weights, err := utils.ReadUint64s("day1/1.txt", utils.IsNewline)
	if err != nil {
		return err
	}

	fmt.Printf("Day 1, Part 1: %d\n", part1(weights))
	fmt.Printf("Day 1, Part 2: %d\n", part2(weights))
	return nil
}
