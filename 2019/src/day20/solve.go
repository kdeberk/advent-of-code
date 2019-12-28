package day20

import (
	"fmt"
	"utils"
)

type coordinate struct {
	x, y, z int
}

func (self coordinate) neighbors() []coordinate {
	return []coordinate{
		coordinate{self.x, self.y - 1, self.z},
		coordinate{self.x - 1, self.y, self.z}, coordinate{self.x + 1, self.y, self.z},
		coordinate{self.x, self.y + 1, self.z},
	}
}

func (self coordinate) withoutZ() coordinate {
	return coordinate{self.x, self.y, 0}
}

func (self coordinate) withZ(z int) coordinate {
	return coordinate{self.x, self.y, z}
}

type portal struct {
	name     string
	position coordinate
	target   coordinate
	inner    bool
}

type maze struct {
	start, end coordinate
	portals    map[coordinate]portal
	grid       [][]rune
}

func readGrid(lines []string) [][]rune {
	grid := make([][]rune, len(lines))

	for y, line := range lines {
		grid[y] = make([]rune, len(line))

		for x, r := range line {
			grid[y][x] = r
		}
	}

	return grid
}

func readPortals(grid [][]rune) (coordinate, coordinate, map[coordinate]portal) {
	portals := make(map[coordinate]portal)

	var start, end coordinate
	named_portals := make(map[string]portal)

	handlePortal := func(name string, point coordinate) {
		if "AA" == name {
			start = point
		} else if "ZZ" == name {
			end = point
		} else {
			inner := 2 < point.y && 2 < point.x && point.x < len(grid[0])-3 && point.y < len(grid)-3

			if other, ok := named_portals[name]; ok {
				portals[point] = portal{name, point, other.position, inner}
				other.target = point
				portals[other.position] = other
			} else {
				named_portals[name] = portal{name, point, coordinate{}, inner}
			}
		}
	}

	for y, row := range grid {
		for x, r := range row {
			if 0 == x || 0 == y {
				continue
			} else if !('A' <= r && r <= 'Z') {
				continue
			}

			{
				prev := grid[y][x-1]

				if 'A' <= prev && prev <= 'Z' {
					name := string([]rune{r, prev})

					var point coordinate
					if x < len(row)-1 && '.' == grid[y][x+1] {
						point = coordinate{x + 1, y, 0}
					} else {
						point = coordinate{x - 2, y, 0}
					}

					handlePortal(name, point)
				}
			}

			{
				prev := grid[y-1][x]

				if 'A' <= prev && prev <= 'Z' {
					name := string([]rune{r, prev})

					var point coordinate
					if y < len(grid)-1 && '.' == grid[y+1][x] {
						point = coordinate{x, y + 1, 0}
					} else {
						point = coordinate{x, y - 2, 0}
					}

					handlePortal(name, point)
				}
			}
		}
	}

	return start, end, portals
}

func readMaze(filename string) (maze, error) {
	lines, err := utils.ReadLines(filename)
	if err != nil {
		return maze{}, err
	}

	maze := maze{}
	maze.grid = readGrid(lines)
	maze.start, maze.end, maze.portals = readPortals(maze.grid)

	return maze, nil
}

type mazeState struct {
	position coordinate
	distance int
}

func part1(maze maze) int {
	seen := make(map[coordinate]bool)
	queue := []mazeState{mazeState{maze.start, 0}}

	var current mazeState
	for 0 < len(queue) {
		current, queue = queue[0], queue[1:]

		if current.position == maze.end {
			break
		} else if _, ok := seen[current.position]; ok {
			continue
		}
		seen[current.position] = true

		if portal, ok := maze.portals[current.position]; ok {
			queue = append(queue, mazeState{portal.target, current.distance + 1})
		}
		for _, neighbor := range current.position.neighbors() {
			if '.' == maze.grid[neighbor.y][neighbor.x] {
				queue = append(queue, mazeState{neighbor, current.distance + 1})
			}
		}
	}

	return current.distance
}

func part2(maze maze) int {
	seen := make(map[coordinate]bool)
	queue := []mazeState{mazeState{maze.start, 0}}

	var current mazeState
	for 0 < len(queue) {
		current, queue = queue[0], queue[1:]

		if current.position == maze.end {
			break
		} else if _, ok := seen[current.position]; ok {
			continue
		}

		seen[current.position] = true

		if portal, ok := maze.portals[current.position.withoutZ()]; ok {
			if portal.inner {
				queue = append(queue, mazeState{portal.target.withZ(current.position.z + 1), current.distance + 1})
			} else if 0 < current.position.z {
				queue = append(queue, mazeState{portal.target.withZ(current.position.z - 1), current.distance + 1})
			}
		}

		for _, neighbor := range current.position.neighbors() {
			if '.' == maze.grid[neighbor.y][neighbor.x] {
				queue = append(queue, mazeState{neighbor, current.distance + 1})
			}
		}
	}

	return current.distance
}

func Solve() error {
	maze, err := readMaze("day20/20.txt")
	if err != nil {
		return err
	}

	fmt.Printf("Day 20, Part 1: %d\n", part1(maze))
	fmt.Printf("Day 20, Part 2: %d\n", part2(maze))

	return nil
}
