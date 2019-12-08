package day8

import (
	"fmt"
	"strings"
	"utils"
)

const imageWidth = 25
const imageHeight = 6
const imageSize = imageWidth * imageHeight

const (
	black       byte = 0
	white       byte = 1
	transparent byte = 2
)

func readDigits(filename string) ([]byte, error) {
	line, err := utils.ReadSingleLine(filename)
	if err != nil {
		return []byte{}, err
	}

	digits := make([]byte, len(line))
	for i, r := range line {
		digits[i] = byte(r) - '0'
	}

	return digits, err
}

func part1(digits []byte) int64 {
	var minZeroCount int64 = 1<<63 - 1
	var answer int64

	for i := 0; i < len(digits)/imageSize; i += 1 {
		layer := digits[i*imageSize : (i+1)*imageSize]

		var counts [10]int64
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

func combineLayers(layers [][]byte) []byte {
	final := make([]byte, len(layers[0]))

	for _, layer := range layers {
		for index, pixel := range layer {
			switch pixel {
			case black:
				final[index] = black
			case white:
				final[index] = white
			case transparent:
			}
		}
	}

	return final
}

func printLayer(layer []byte) string {
	builder := strings.Builder{}
	for index := 0; index < imageSize; {
		for rowIndex := 0; rowIndex < imageWidth; rowIndex, index = rowIndex+1, index+1 {
			switch layer[index] {
			case black:
				builder.WriteRune(' ')
			case white:
				builder.WriteRune('#')
			case transparent:
			}
		}
		builder.WriteRune('\n')
	}

	return builder.String()
}

func part2(digits []byte) string {
	nLayers := len(digits) / imageSize

	layers := make([][]byte, nLayers)
	for i := 0; i < nLayers; i += 1 {
		layer := digits[i*imageSize : (i+1)*imageSize]
		layers[nLayers-i-1] = layer
	}

	combined := combineLayers(layers)
	return printLayer(combined)
}

func Solve() error {
	digits, err := readDigits("day8/8.txt")
	if err != nil {
		return err
	}

	fmt.Printf("Day 8, Part 1: %d\n", part1(digits))
	fmt.Printf("Day 8, Part 2:\n%s\n", part2(digits))
	return nil
}
