package day15

import (
	"fmt"
	"strings"
	"time"

	"github.com/kdeberk/advent-of-code/2019/internal/config"
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const size = 22

type coordinate struct {
	x, y int
}

// TODO: replace map with array

func (self coordinate) neighbor(d direction) coordinate {
	dx := map[direction]int{north: 0, south: 0, east: -1, west: 1}[d]
	dy := map[direction]int{north: -1, south: 1, east: 0, west: 0}[d]

	return coordinate{self.x + dx, self.y + dy}
}

func (self coordinate) neighbors() []coordinate {
	neighbors := make([]coordinate, 4)

	for i, d := range []direction{north, south, east, west} {
		neighbors[i] = self.neighbor(d)
	}
	return neighbors
}

type direction int64
type reply int64
type thing int

const (
	north direction = 1
	south           = 2
	west            = 3
	east            = 4
)

const (
	hitWall             reply = 0
	moved                     = 1
	movedAndFoundOxygen       = 2
)

const (
	unexplored = 0
	wall       = 1
	open       = 2
	oxygen     = 3
)

type remoteControl struct {
	computer     utils.Machine
	droid        coordinate
	oxygenSystem coordinate
	field        map[coordinate]thing
}

func newRemoteControl(program utils.Program) *remoteControl {
	computer := utils.MakeMachine("day15", program)

	droid := coordinate{0, 0}
	field := make(map[coordinate]thing, 2*2*size*size)

	for x := -size; x < size; x += 1 {
		for y := -size; y < size; y += 1 {
			field[coordinate{x, y}] = unexplored
		}
	}
	field[droid] = open

	return &remoteControl{
		computer,
		droid,
		coordinate{},
		field,
	}
}

type routeState struct {
	position coordinate
	steps    []direction
}

func routeToNearest(position coordinate, thing thing, field map[coordinate]thing) ([]direction, bool) {
	queue := []routeState{}
	seen := make(map[coordinate]bool)

	queue = append(queue, routeState{position, []direction{}})

	var current routeState
	for 0 < len(queue) {
		current, queue = queue[0], queue[1:]

		if thing == field[current.position] {
			return current.steps, true
		}

		if _, found := seen[current.position]; found {
			continue
		} else {
			seen[current.position] = true
		}

		for _, d := range []direction{north, east, west, south} {
			neighbor := current.position.neighbor(d)

			if _, found := seen[neighbor]; found {
				continue
			} else if wall == field[neighbor] {
				continue
			}

			new_steps := make([]direction, len(current.steps))
			copy(new_steps, current.steps)
			queue = append(queue, routeState{neighbor, append(new_steps, d)})
		}
	}

	return []direction{}, false
}

func (self *remoteControl) render() {
	builder := strings.Builder{}
	for x := -size; x < size; x += 1 {
		for y := -size; y < size; y += 1 {
			c := coordinate{x, y}
			if c == self.droid {
				builder.WriteRune('D')
				continue
			}

			switch self.field[c] {
			case unexplored:
				builder.WriteRune('~')
			case wall:
				builder.WriteRune('#')
			case open:
				builder.WriteRune(' ')
			case oxygen:
				builder.WriteRune('O')
			}
		}
		builder.WriteRune('\n')
	}

	print("\033[H\033[2J")
	fmt.Println(builder.String())
}

func (self *remoteControl) explore() {
	go self.computer.Run()

ExploreLoop:
	for {
		steps, found := routeToNearest(self.droid, unexplored, self.field)
		if false == found {
			break ExploreLoop
		}

		for _, step := range steps {
			next := self.droid.neighbor(step)
			self.computer.Input <- int64(step)

			switch reply(<-self.computer.Output) {
			case hitWall:
				self.field[next] = wall
			case moved:
				self.droid = next
				self.field[next] = open
			case movedAndFoundOxygen:
				self.droid = next
				self.field[next] = oxygen
				self.oxygenSystem = next
			}
		}
		if config.Render {
			self.render()
			time.Sleep(10 * time.Millisecond)
		}
	}
}

func part1(remote *remoteControl) int {
	remote.explore()
	steps, _ := routeToNearest(coordinate{0, 0}, oxygen, remote.field)
	return len(steps)
}

type depthState struct {
	position coordinate
	depth    int
}

func part2(remote *remoteControl) int {
	queue := []depthState{}

	queue = append(queue, depthState{remote.oxygenSystem, 0})

	var current depthState
	for 0 < len(queue) {
		current, queue = queue[0], queue[1:]

		switch {
		case remote.oxygenSystem == current.position:
			break
		case oxygen == remote.field[current.position]:
			continue
		default:
			remote.field[current.position] = oxygen
		}

		for _, d := range []direction{north, east, west, south} {
			neighbor := current.position.neighbor(d)

			switch remote.field[neighbor] {
			case oxygen, wall:
				continue
			}

			queue = append(queue, depthState{neighbor, current.depth + 1})

			if config.Render {
				remote.render()
				time.Sleep(10 * time.Millisecond)
			}

		}
	}
	return current.depth
}

func Solve() error {
	program, err := utils.ReadProgram("./input/15.txt")
	if err != nil {
		return err
	}

	remote := newRemoteControl(program)
	fmt.Println("Day 15, Part 1. Use a blind robot to find the Oxygen system in a maze")
	fmt.Println(" ", part1(remote))
	fmt.Println("Day 15, Part 2: Turn on the Oxygen system and wait until the maze is filled.")
	fmt.Println(" ", part2(remote))

	return nil
}
