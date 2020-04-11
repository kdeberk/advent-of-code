package day8

import (
	"fmt"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

func part1(layers []layer) int {
	var minZeroCount int = 1<<63 - 1
	var answer int

	for _, layer := range layers {
		var counts [10]int

		for _, pixel := range layer {
			counts[pixel] += 1
		}

		if counts[0] < minZeroCount {
			minZeroCount = counts[0]
			answer = counts[1] * counts[2]
		}
	}
	return answer
}

func part2(layers []layer) string {
	return utils.TrimASCIIArt(renderLayer(combineLayers(layers)))
}

func Solve() error {
	layers, err := readLayers("./input/8.txt")
	if err != nil {
		return err
	}

	fmt.Println("Day 8, Part 1. Receive an image of the BIOS password of a Mars Rover. This image is in layers. Determine the layer with the most zeroes.")
	fmt.Println(" ", part1(layers))
	fmt.Println("Day 8, Part 2. Combine the image layers to produce an image that displays the BIOS password for the Mars Rover.")
	fmt.Println(utils.Prefix(part2(layers), " "))
	return nil
}
