package day8

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 1320
const part2Answer = `
###   ##  #   ##  # ###
#  # #  # #   ## #  #  #
#  # #     # # ##   #  #
###  #      #  # #  ###
# #  #  #   #  # #  # #
#  #  ##    #  #  # #  #
`

func TestPart1(t *testing.T) {
	layers, err := readLayers("../../input/8.txt")
	if err != nil {
		t.Fatal("Could not read layers", err)
	}
	answer := part1(layers)

	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	layers, err := readLayers("../../input/8.txt")
	if err != nil {
		t.Fatal("Could not read layers", err)
	}

	expected := utils.TrimASCIIArt(part2Answer)

	answer := part2(layers)
	if expected != answer {
		t.Errorf("part2(input) == \n%v, want\n%v", answer, expected)
	}
}
