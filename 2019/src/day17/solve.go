package day17

import (
	"fmt"
	"strconv"
	"strings"
	"utils"
)

type scaffold []string
type direction int

const (
	north direction = iota
	south           = iota
	west            = iota
	east            = iota
)

type droid struct {
	x, y      int
	direction direction
}

func (self scaffold) render() string {
	builder := strings.Builder{}
	for _, row := range self {
		for _, r := range row {
			builder.WriteRune(r)
		}
		builder.WriteRune('\n')
	}
	return builder.String()
}

func (self scaffold) countIntersections() int {
	var sum int

	for y := 1; y < len(self)-1; y += 1 {
		for x := 1; x < len(self[y])-1; x += 1 {
			if '.' == self[y-1][x-1] && '#' == self[y-1][x] && '.' == self[y-1][x+1] &&
				'#' == self[y][x-1] && '#' == self[y][x] && '#' == self[y][x+1] &&
				'.' == self[y+1][x-1] && '#' == self[y+1][x] && '.' == self[y+1][x+1] {
				sum += x * y
			}
		}
	}

	return sum
}

func (self scaffold) locateDroid() droid {
	var x, y int
	var direction direction

LocateDroidLoop:
	for ; y < len(self); y += 1 {
		for ; x < len(self[y]); x += 1 {
			switch self[y][x] {
			case '^':
				direction = north
			case '<':
				direction = west
			case '>':
				direction = east
			case 'v':
				direction = south
			default:
				continue
			}
			break LocateDroidLoop
		}
	}
	return droid{x, y, direction}
}

func (self droid) lookForward(scaffold scaffold) bool {
	x := self.x + map[direction]int{north: 0, south: 0, west: -1, east: 1}[self.direction]
	y := self.y + map[direction]int{north: -1, south: 1, west: 0, east: 0}[self.direction]

	return 0 <= y && y < len(scaffold) && 0 <= x && x < len(scaffold[y]) && '#' == scaffold[y][x]
}

func (self droid) lookLeft(scaffold scaffold) bool {
	x := self.x + map[direction]int{north: -1, south: 1, west: 0, east: 0}[self.direction]
	y := self.y + map[direction]int{north: 0, south: 0, west: 1, east: -1}[self.direction]

	return 0 <= y && y < len(scaffold) && 0 <= x && x < len(scaffold[y]) && '#' == scaffold[y][x]
}

func (self droid) lookRight(scaffold scaffold) bool {
	x := self.x + map[direction]int{north: 1, south: 0, west: 0, east: 0}[self.direction]
	y := self.y + map[direction]int{north: 0, south: 0, west: -1, east: 1}[self.direction]

	return 0 <= y && y < len(scaffold) && 0 <= x && x < len(scaffold[y]) && '#' == scaffold[y][x]
}

func (self *droid) moveForward() {
	self.x += map[direction]int{north: 0, south: 0, west: -1, east: 1}[self.direction]
	self.y += map[direction]int{north: -1, south: 1, west: 0, east: 0}[self.direction]
}

func (self *droid) turnLeft() {
	self.direction = map[direction]direction{north: west, west: south, south: east, east: north}[self.direction]
}

func (self *droid) turnRight() {
	self.direction = map[direction]direction{north: east, east: south, south: west, west: north}[self.direction]
}

type step struct {
	direction rune
	n_steps   int
}

type path []step

func (self *droid) navigateScaffold(scaffold scaffold) path {
	steps := []step{}

NavigateLoop:
	for {
		var current step

		if self.lookLeft(scaffold) {
			self.turnLeft()
			current.direction = 'L'
		} else if self.lookRight(scaffold) {
			self.turnRight()
			current.direction = 'R'
		} else {
			break NavigateLoop
		}

		for self.lookForward(scaffold) {
			current.n_steps += 1
			self.moveForward()
		}
		steps = append(steps, current)
	}

	return path(steps)
}

func (self path) startsWithFragment(fragment []step) bool {
	if len(self) < len(fragment) {
		return false
	}
	for i := range fragment {
		if self[i] != fragment[i] {
			return false
		}
	}
	return true
}

func (self path) String() string {
	result := []string{}

	for _, step := range self {
		result = append(result, string(step.direction), strconv.Itoa(step.n_steps))
	}

	return strings.Join(result, ",")
}

func findFragments(path path, fragments []path, maxNFragments, maxStringLength int) []path {
	if 0 == len(path) {
		return fragments
	}

	var matched []step
	for _, function := range fragments {
		if path.startsWithFragment(function) {
			matched = function
		}
	}

	if nil != matched {
		return findFragments(path[len(matched):], fragments, maxNFragments, maxStringLength)
	} else if maxNFragments == len(fragments) {
		return nil
	}

	current := path[:1]
	for len(current.String()) <= maxStringLength {
		result := findFragments(path[len(current):], append(fragments, current), maxNFragments, maxStringLength)
		if nil != result {
			return result
		}
		current = path[:len(current)+1]
	}
	return nil
}

func compressPath(full_path path) ([]string, []path) {
	main := []string{}
	functions := findFragments(full_path, []path{}, 3, 20)

ReduceFullPathLoop:
	for 0 < len(full_path) {
		for i, function := range functions {
			if full_path.startsWithFragment(function) {
				main = append(main, string('A'+i))
				full_path = full_path[len(function):]
				continue ReduceFullPathLoop
			}
		}
	}
	return main, functions
}

func answerPrompt(robot *utils.AsciiMachine, expected, answer string) error {
	prompt, err := robot.ReceiveUntil("\n")
	if err != nil {
		return err
	} else if prompt[:len(prompt)-2] != expected {
		return fmt.Errorf("Received prompt: %s, expected: %s", prompt[:len(prompt)-2], expected)
	}
	robot.SendString(answer + "\n")
	return nil
}

func part1(program utils.Program) (int, error) {
	robot := utils.MakeAsciiMachine("day17", program)
	go robot.Run()

	received, err := robot.ReceiveUntilTermination()
	if err != nil {
		return 0, err
	}

	return scaffold(strings.Split(received, "\n")).countIntersections(), nil
}

func part2(program utils.Program) (int, error) {
	robot := utils.MakeAsciiMachine("day17", program)
	robot.SetMemory(0, 2)
	go robot.Run()

	received, err := robot.ReceiveUntil("\n\n")
	if err != nil {
		return 0, err
	}

	scaffold := scaffold(strings.Split(received[:len(received)-2], "\n"))
	droid := scaffold.locateDroid()
	path := droid.navigateScaffold(scaffold)
	main, functions := compressPath(path)

	err = answerPrompt(&robot, "Main", strings.Join(main, ","))
	if err != nil {
		return 0, err
	}
	for i, name := range []string{"A", "B", "C"} {
		err = answerPrompt(&robot, "Function "+name, functions[i].String())
		if err != nil {
			return 0, err
		}
	}
	err = answerPrompt(&robot, "Continuous video feed", "N")
	if err != nil {
		return 0, err
	}

	var answer int64
ReadAnswerLoop:
	for {
		select {
		case err := <-robot.Error:
			if err != nil {
				return 0, err
			} else {
				break ReadAnswerLoop
			}
		case answer = <-robot.Output:
		}
	}

	return int(answer), nil
}

func Solve() error {
	program, err := utils.ReadProgram("day17/17.txt")
	if err != nil {
		return err
	}

	var answer int
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 17, Part 1:", answer)

	answer, err = part2(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 17, Part 2:", answer)
	return nil
}
