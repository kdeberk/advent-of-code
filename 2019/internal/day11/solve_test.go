package day11

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 1967
const part2Answer = `
#  # ###  #  # ####  ##  #### ###  #  #
# #  #  # #  # #    #  #    # #  # # #
##   ###  #  # ###  #      #  ###  ##
# #  #  # #  # #    # ##  #   #  # # #
# #  #  # #  # #    #  # #    #  # # #
#  # ###   ##  ####  ### #### ###  #  #
`

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("../../input/11.txt")
	if err != nil {
		t.Fatal("Could not open grid", err)
	}

	answer, _ := part1(program)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("../../input/11.txt")
	if err != nil {
		t.Fatal("Could not open grid", err)
	}

	expected := utils.TrimASCIIArt(part2Answer)

	answer, _ := part2(program)
	if expected != answer {
		t.Errorf("part2(input) == \n%v, want\n%v", answer, expected)
	}
}
