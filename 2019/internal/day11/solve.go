package day11

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
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

func (r *robot) turnLeft() {
	r.direction = map[direction]direction{
		north: west, west: south, south: east, east: north,
	}[r.direction]
}

func (r *robot) turnRight() {
	r.direction = map[direction]direction{
		north: east, west: north, south: west, east: south,
	}[r.direction]
}

func (r *robot) moveOneStep() {
	dx := map[direction]int64{north: 0, south: 0, east: 1, west: -1}[r.direction]
	dy := map[direction]int64{north: -1, south: 1, east: 0, west: 0}[r.direction]

	r.location = coordinate{r.location.x + dx, r.location.y + dy}
}

func (r *robot) handleData(data int64, target surface) error {
	switch r.state {
	case painting:
		target[r.location] = color(data)
		r.state = moving
	case moving:
		switch data {
		case 0:
			r.turnLeft()
		case 1:
			r.turnRight()
		default:
			return fmt.Errorf("Unknown direction: %d", data)
		}
		r.moveOneStep()
		r.state = painting
	}
	return nil
}

func (r *robot) paintHull(hull surface, render bool) error {
	go r.computer.Run()

PaintLoop:
	for {
		select {
		case r.computer.Input <- int64(hull[r.location]):
		case output := <-r.computer.Output:
			r.handleData(output, hull)
			if render {
				fmt.Println("\033[H\033[2J")
				fmt.Println(renderHull(hull, r))
				time.Sleep(20 * time.Millisecond)
			}
		case err := <-r.computer.Error:
			if err != nil {
				return err
			} else {
				break PaintLoop
			}
		}
	}
	return nil
}

type RuneWriter interface {
	WriteRune(r rune) (int, error)
}

func (r *robot) render(w RuneWriter) {
	switch r.direction {
	case east:
		w.WriteRune('>')
	case west:
		w.WriteRune('<')
	case north:
		w.WriteRune('^')
	case south:
		w.WriteRune('v')
	}
}

func part1(program utils.Program) (int64, error) {
	machine := utils.MakeMachine("day11", program)

	hull := surface{}
	painter := robot{machine, coordinate{0, 0}, north, painting}
	painter.paintHull(hull, false)

	return int64(len(hull)), nil
}

func renderHull(hull surface, r *robot) string {
	builder := &strings.Builder{}
	for y := int64(0); y < 6; y++ {
		for x := int64(0); x < 40; x++ {
			switch {
			case r != nil && x == r.location.x && y == r.location.y:
				r.render(builder)
			case white == hull[coordinate{x, y}]:
				builder.WriteRune('#')
			default:
				builder.WriteRune(' ')
			}
		}
		builder.WriteRune('\n')
	}

	return builder.String()
}

func part2(program utils.Program) (string, error) {
	render := "1" == os.Getenv("AOC_RENDER")
	machine := utils.MakeMachine("day11", program)

	hull := surface{}
	painter := &robot{machine, coordinate{0, 0}, north, painting}
	hull[painter.location] = white
	painter.paintHull(hull, render)

	return utils.TrimASCIIArt(renderHull(hull, painter)), nil
}

func Solve() error {
	program, err := utils.ReadProgram("./input/11.txt")
	if err != nil {
		return err
	}

	answer1, err := part1(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 11, Part 1. To prevent arrest by the space police, paint the hulls. The amount of cells painted:")
	fmt.Println(" ", answer1)

	answer2, err := part2(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 11, Part 2. Render the painted hull.")
	fmt.Println(answer2)
	return nil
}
