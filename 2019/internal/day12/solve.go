package day12

import (
	"fmt"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

type vector [3]int

func abs(a int) int {
	if 0 < a {
		return -a
	} else {
		return a
	}
}

func (self vector) length() int {
	return abs(self[0]) + abs(self[1]) + abs(self[2])
}

func (self *vector) add(other vector) {
	for i := 0; i < 3; i++ {
		self[i] += other[i]
	}
}

type moon struct {
	position, velocity vector
}

func (self moon) potentialEnergy() int {
	return self.position.length()
}

func (self moon) kineticEnergy() int {
	return self.velocity.length()
}

func applyGravity(moons []moon) {
	for i := range moons {
		for j := range moons[i+1:] {
			j += i + 1

			for v := 0; v < 3; v += 1 {
				if moons[i].position[v] < moons[j].position[v] {
					moons[i].velocity[v] += 1
					moons[j].velocity[v] -= 1
				} else if moons[j].position[v] < moons[i].position[v] {
					moons[i].velocity[v] -= 1
					moons[j].velocity[v] += 1
				}
			}
		}
	}
}

func applyVelocity(moons []moon) {
	for i := range moons {
		moons[i].position.add(moons[i].velocity)
	}
}

func totalEnergy(moons []moon) int {
	var totalEnergy int
	for _, moon := range moons {
		totalEnergy += moon.potentialEnergy() * moon.kineticEnergy()
	}
	return totalEnergy
}

func part1(initial []moon) int {
	moons := make([]moon, len(initial))
	copy(moons, initial)

	for i := 0; i < 1000; i++ {
		applyGravity(moons)
		applyVelocity(moons)
	}
	return totalEnergy(moons)
}

func findPeriod(start []moon, axis int) int {
	moons := make([]moon, len(start))
	copy(moons, start)

	initial := make([]int, 2*len(moons))
	for i, moon := range moons {
		initial[2*i] = moon.position[axis]
		initial[2*i+1] = moon.velocity[axis]
	}

	var period int
PeriodSearch:
	for period = 1; ; period++ {
		applyGravity(moons)
		applyVelocity(moons)

		for i, moon := range moons {
			if moon.position[axis] != initial[2*i] || moon.velocity[axis] != initial[2*i+1] {
				continue PeriodSearch
			}
		}
		break PeriodSearch
	}
	return period
}

func part2(moons []moon) int {
	return utils.LCM(findPeriod(moons, 0), findPeriod(moons, 1), findPeriod(moons, 2))
}

func Solve() error {
	moons := []moon{
		moon{vector{17, -12, 13}, vector{}},
		moon{vector{2, 1, 1}, vector{}},
		moon{vector{-1, -17, 7}, vector{}},
		moon{vector{12, -14, 18}, vector{}},
	}

	fmt.Println("Day 12, Part 1. The total energy stored in the orbits of 4 moons after 1000 steps.")
	fmt.Println(" ", part1(moons))
	fmt.Println("Day 12, Part 2. The length of the entire orbital cycle in steps.")
	fmt.Println(" ", part2(moons))

	return nil
}
