package day11

import (
	"fmt"
	"strings"
	"utils"
)

type direction byte
type state byte
type color int64

const (
	white = 1
	black = 0
)

const (
	north direction = iota
	south           = iota
	east            = iota
	west            = iota
)

const (
	painting state = iota
	moving         = iota
)

type coordinate struct {
	x int64
	y int64
}

type surface map[coordinate]color

type robot struct {
	computer  utils.Machine
	location  coordinate
	direction direction
	state     state
}

func (self *robot) turnLeft() {
	self.direction = map[direction]direction{
		north: west, west: south, south: east, east: north,
	}[self.direction]
}

func (self *robot) turnRight() {
	self.direction = map[direction]direction{
		north: east, west: north, south: west, east: south,
	}[self.direction]
}

func (self *robot) moveOneStep() {
	dx := map[direction]int64{north: 0, south: 0, east: 1, west: -1}[self.direction]
	dy := map[direction]int64{north: -1, south: 1, east: 0, west: 0}[self.direction]

	self.location = coordinate{self.location.x + dx, self.location.y + dy}
}

func (self *robot) handleData(data int64, target surface) error {
	switch self.state {
	case painting:
		target[self.location] = color(data)
		self.state = moving
	case moving:
		switch data {
		case 0:
			self.turnLeft()
		case 1:
			self.turnRight()
		default:
			return fmt.Errorf("Unknown direction: %d", data)
		}
		self.moveOneStep()
		self.state = painting
	}
	return nil
}

func (self *robot) paintHull(hull surface) error {
	go self.computer.Run()

PaintLoop:
	for {
		select {
		case self.computer.Input <- int64(hull[self.location]):
		case output := <-self.computer.Output:
			self.handleData(output, hull)
		case err := <-self.computer.Error:
			if err != nil {
				return err
			} else {
				break PaintLoop
			}
		}
	}
	return nil
}

func part1(program utils.Program) (int64, error) {
	machine := utils.MakeMachine("day11")
	machine.LoadProgram(program)

	hull := surface{}
	painter := robot{machine, coordinate{0, 0}, north, painting}
	painter.paintHull(hull)

	return int64(len(hull)), nil
}

func renderHull(hull surface) string {
	builder := strings.Builder{}
	for y := int64(0); y < 6; y++ {
		for x := int64(0); x < 40; x++ {
			if white == hull[coordinate{x, y}] {
				builder.WriteRune('#')
			} else {
				builder.WriteRune(' ')
			}
		}
		builder.WriteRune('\n')
	}

	return builder.String()
}

func part2(program utils.Program) (string, error) {
	machine := utils.MakeMachine("day11")
	machine.LoadProgram(program)

	hull := surface{}
	painter := robot{machine, coordinate{0, 0}, north, painting}
	hull[painter.location] = white
	painter.paintHull(hull)

	return renderHull(hull), nil
}

func Solve() error {
	program, err := utils.ReadProgram("day11/11.txt")
	if err != nil {
		return err
	}

	var answer int64
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Printf("Day 11, Part 1: %d\n", answer)

	answer2, err := part2(program)
	if err != nil {
		return err
	}
	fmt.Printf("Day 11, Part 2:\n%s\n", answer2)
	return nil
}
