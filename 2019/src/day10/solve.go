package day10

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"time"
	"utils"
)

type asteroid struct {
	x float64
	y float64
}

func abs(a float64) float64 {
	if a < 0 {
		return -a
	} else {
		return a
	}
}

func (self asteroid) distance(other asteroid) float64 {
	return abs(self.x-other.x) + abs(self.y-other.y)
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
		return []asteroid{}, err
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

func printAsteroidField(laser asteroid, group map[slope][]asteroid) {
	print("\033[H\033[2J")

	field := make([][]bool, 40)
	for i := range field {
		field[i] = make([]bool, 40)
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
				builder.WriteRune('L')
			} else if true == field[x][y] {
				builder.WriteRune('#')
			} else {
				builder.WriteRune('.')
			}
		}
		builder.WriteRune('\n')
	}
	fmt.Printf(builder.String())
}

func countUniques(slopes []slope) int {
	uniques := map[slope]bool{}

	for _, slope := range slopes {
		uniques[slope] = true
	}
	return len(uniques)
}

func bestAsteroid(asteroids []asteroid) (asteroid, int) {
	var maxSeen int
	var best asteroid

	for i, asteroid := range asteroids {
		slopes := []slope{}

		for j, other := range asteroids {
			if i == j {
				continue
			}

			slopes = append(slopes, asteroid.calculateSlope(other))
		}

		countSeen := countUniques(slopes)
		if maxSeen < countSeen {
			maxSeen = countSeen
			best = asteroid
		}
	}

	return best, maxSeen
}

func part1(asteroids []asteroid) int {
	_, nSeen := bestAsteroid(asteroids)
	return nSeen
}

func part2(asteroids []asteroid) int {
	laser, _ := bestAsteroid(asteroids)

	groups := make(map[slope][]asteroid)
	for _, asteroid := range asteroids {
		if asteroid == laser {
			continue
		}

		slope := laser.calculateSlope(asteroid)
		groups[slope] = append(groups[slope], asteroid)
	}

	slopes := make([]slope, len(groups))
	for slope := range groups {
		slopes = append(slopes, slope)
		sort.SliceStable(groups[slope], func(i, j int) bool {
			return laser.distance(groups[slope][i]) < laser.distance(groups[slope][j])
		})
	}

	sort.SliceStable(slopes, func(i, j int) bool {
		if slopes[i].quadrant == slopes[j].quadrant {
			return slopes[j].angle < slopes[i].angle
		} else {
			return slopes[i].quadrant < slopes[j].quadrant
		}
	})

	var nBlownUp int
	var blownUp asteroid
SearchAndDestroy:
	for {
		for _, slope := range slopes {
			if 0 < len(groups[slope]) {
				blownUp, groups[slope] = groups[slope][0], groups[slope][1:]
				nBlownUp += 1

				if "1" == os.Getenv("AOC_RENDER") {
					printAsteroidField(laser, groups)
					time.Sleep(100 * time.Millisecond)
				}

				if 200 == nBlownUp {
					break SearchAndDestroy
				}
			}
		}
	}

	return 100*int(blownUp.x) + int(blownUp.y)
}

func Solve() error {
	asteroids, err := readAsteroids("day10/10.txt")
	if err != nil {
		return err
	}

	fmt.Printf("Day 10, Part 1: %d\n", part1(asteroids))
	fmt.Printf("Day 10, Part 2: %d\n", part2(asteroids))

	return nil
}
