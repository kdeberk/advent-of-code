package day3

import (
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

type point struct {
	x, y int
}

func (self point) distance(other point) uint {
	return uint(utils.AbsInt(self.x-other.x)) + uint(utils.AbsInt(self.y-other.y))
}
