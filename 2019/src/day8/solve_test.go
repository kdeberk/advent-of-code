package day8

import (
	"testing"
)

const part1Answer = 1320
const part2Answer = `###   ##  #   ##  # ###  
#  # #  # #   ## #  #  # 
#  # #     # # ##   #  # 
###  #      #  # #  ###  
# #  #  #   #  # #  # #  
#  #  ##    #  #  # #  # `

func TestPart1(t *testing.T) {
	layers, _ := readLayers("8.txt")
	answer := part1(layers)

	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	layers, _ := readLayers("8.txt")
	answer := part2(layers)

	if part2Answer != answer {
		t.Errorf("part2(input) == \n%v, want\n%v", answer, part2Answer)
	}
}
