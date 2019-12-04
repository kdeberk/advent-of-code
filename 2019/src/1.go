package main

import (
	"fmt"
	"utils"
)

const part1Answer = 3235550
const part2Answer = 4850462

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

func main() {
	weights, err := utils.ReadUint64s("../data/1.txt", utils.IsNewline)
	if err != nil {
		fmt.Println(err.Error())
		return
	}

	var answer uint64
	answer = part1(weights)
	if part1Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part1Answer))
	}
	fmt.Printf("Part 1: %d\n", answer)

	answer = part2(weights)
	if part2Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part2Answer))
	}
	fmt.Printf("Part 2: %d\n", answer)
}
