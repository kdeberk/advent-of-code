package day10

import (
	"fmt"
	"os"
	"sort"
	"time"
)

func bestAsteroid(asteroids []asteroid) (asteroid, int) {
	var maxSeen int
	var best asteroid

	// TODO: this is O(N^2), perhaps we can do better?
	for i, asteroid := range asteroids {
		slopes := make(map[slope]bool)

		for j, other := range asteroids {
			if i == j {
				continue
			}

			slopes[asteroid.calculateSlope(other)] = true
		}

		if maxSeen < len(slopes) {
			maxSeen = len(slopes)
			best = asteroid
		}
	}

	return best, maxSeen
}

func part1(asteroids []asteroid) int {
	_, nSeen := bestAsteroid(asteroids)
	return nSeen
}

func groupAsteroidsBySlope(laser asteroid, asteroids []asteroid) (map[slope][]asteroid, []slope) {
	// Group other asteroids by their slope wrt to the Laser
	groups := make(map[slope][]asteroid)
	for _, asteroid := range asteroids {
		if asteroid == laser {
			continue
		}

		slope := laser.calculateSlope(asteroid)
		groups[slope] = append(groups[slope], asteroid)
	}

	// For each slope sort all asteroids by their distance, closest first.
	for slope := range groups {
		sort.SliceStable(groups[slope], func(i, j int) bool {
			return laser.distance(groups[slope][i]) < laser.distance(groups[slope][j])
		})
	}

	// Obtain all unique slopes (i.e. the keys of the groups)
	slopes := make([]slope, len(groups))
	for slope := range groups {
		slopes = append(slopes, slope)
	}

	// Sort the list of slopes so that it forms a circle around the laser.
	sort.SliceStable(slopes, func(i, j int) bool {
		if slopes[i].quadrant == slopes[j].quadrant {
			return slopes[j].angle < slopes[i].angle
		} else {
			return slopes[i].quadrant < slopes[j].quadrant
		}
	})

	return groups, slopes
}

func part2(asteroids []asteroid) int {
	render := "1" == os.Getenv("AOC_RENDER")

	laser, _ := bestAsteroid(asteroids)
	groups, slopes := groupAsteroidsBySlope(laser, asteroids)

	var nBlownUp int
	var blownUp asteroid
SearchAndDestroy:
	for {
		for _, slope := range slopes {
			if 0 < len(groups[slope]) {
				blownUp, groups[slope] = groups[slope][0], groups[slope][1:]
				nBlownUp += 1

				if true == render {
					print("\033[H\033[2J\n")
					fmt.Println("Asteroids blown up: ", nBlownUp)
					fmt.Println(renderAsteroidField(laser, blownUp, groups))
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
