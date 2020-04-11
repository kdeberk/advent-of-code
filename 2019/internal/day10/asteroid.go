package day10

import (
	"math"
	"strings"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

type asteroid struct {
	x float64
	y float64
}

func (self asteroid) distance(other asteroid) float64 {
	return math.Abs(self.x-other.x) + math.Abs(self.y-other.y)
}

type slope struct {
	quadrant byte
	angle    float64
}

func (self asteroid) calculateSlope(other asteroid) slope {
	var quadrant byte
	if self.x <= other.x && other.y < self.y {
		quadrant = 1
	} else if self.x <= other.x && self.y <= other.y {
		quadrant = 2
	} else if other.x < self.x && self.y <= other.y {
		quadrant = 3
	} else {
		quadrant = 4
	}

	return slope{quadrant, (self.x - other.x) / (self.y - other.y)}
}

func readAsteroids(filename string) ([]asteroid, error) {
	lines, err := utils.ReadLines(filename)
	if err != nil {
		return nil, err
	}

	asteroids := []asteroid{}
	for y, line := range lines {
		for x, r := range line {
			if '#' == r {
				asteroids = append(asteroids, asteroid{float64(x) + 0.5, float64(y) + 0.5})
			}
		}
	}

	return asteroids, nil
}

func renderAsteroidField(laser, target asteroid, group map[slope][]asteroid) string {
	field := make([][]bool, 36)
	for i := range field {
		field[i] = make([]bool, 36)
	}

	for _, asteroids := range group {
		for _, asteroid := range asteroids {
			field[int(asteroid.x)][int(asteroid.y)] = true
		}
	}

	builder := strings.Builder{}
	for y := 0; y < len(field[0]); y++ {
		for x := 0; x < len(field); x++ {
			if x == int(laser.x) && y == int(laser.y) {
				builder.WriteString("\033[1;31mL\033[0m")
			} else if x == int(target.x) && y == int(target.y) {
				builder.WriteString("\033[1;31m#\033[0m")
			} else if true == field[x][y] {
				builder.WriteRune('#')
			} else {
				builder.WriteRune('.')
			}
		}
		builder.WriteRune('\n')
	}

	return builder.String()
}
