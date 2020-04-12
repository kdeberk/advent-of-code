package day18

import (
	"fmt"
	"strings"
	"time"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

type coordinate struct {
	x, y int
}

// TODO: this could be fun to visualize

func (self coordinate) neighbors() []coordinate {
	return []coordinate{
		coordinate{self.x, self.y - 1},
		coordinate{self.x - 1, self.y}, coordinate{self.x + 1, self.y},
		coordinate{self.x, self.y + 1},
	}
}

type path struct {
	from, to    rune
	key         int64
	distance    int
	key_bitmap  int64
	door_bitmap int64
}

type key struct {
	coordinate coordinate
	name       rune
	paths      []path
}

func makeKey(coordinate coordinate, name rune) key {
	return key{coordinate, name, []path{}}
}

type entrance struct {
	coordinate coordinate
	paths      []path
}

func makeEntrance(coordinate coordinate) entrance {
	return entrance{coordinate, []path{}}
}

type maze struct {
	entrances []entrance
	keys      map[rune]key
	grid      [][]rune
}

func newMaze(height, width int) *maze {
	grid := make([][]rune, height)
	for y := 0; y < height; y += 1 {
		grid[y] = make([]rune, width)
	}

	return &maze{[]entrance{}, make(map[rune]key, 0), grid}
}

type gatherState struct {
	coordinate  coordinate
	distance    int
	key_bitmap  int64
	door_bitmap int64
}

func gatherPaths(from rune, start coordinate, maze *maze) []path {
	seen := make(map[coordinate]bool, len(maze.grid)*len(maze.grid[0]))
	queue := []gatherState{}
	paths := []path{}

	queue = append(queue, gatherState{start, 0, 0, 0})

	var current gatherState
	for 0 < len(queue) {
		current, queue = queue[0], queue[1:]

		if _, ok := seen[current.coordinate]; ok {
			continue
		}
		seen[current.coordinate] = true

		r := maze.grid[current.coordinate.y][current.coordinate.x]
		if 'a' <= r && r <= 'z' && from != r {
			paths = append(paths, path{from, r, 1 << (r - 'a'), current.distance, current.key_bitmap, current.door_bitmap})
			current.key_bitmap = current.key_bitmap | 1<<(r-'a')
		} else if 'A' <= r && r <= 'Z' {
			current.door_bitmap = current.door_bitmap | 1<<(r-'A')
		}

		for _, neighbor := range current.coordinate.neighbors() {
			if _, ok := seen[neighbor]; ok {
				continue
			} else if '#' == maze.grid[neighbor.y][neighbor.x] {
				continue
			} else {
				queue = append(queue, gatherState{neighbor, current.distance + 1, current.key_bitmap, current.door_bitmap})
			}
		}
	}

	return paths
}

func readMaze(filename string) (*maze, error) {
	lines, err := utils.ReadLines(filename)
	if err != nil {
		return newMaze(0, 0), err
	}

	maze := newMaze(len(lines), len(lines[0]))

	for y, line := range lines {
		for x, r := range line {
			maze.grid[y][x] = r

			switch {
			case '@' == r:
				maze.entrances = append(maze.entrances, makeEntrance(coordinate{x, y}))
			case 'a' <= r && r <= 'z':
				maze.keys[r] = makeKey(coordinate{x, y}, r)
			}
		}
	}

	for name, key := range maze.keys {
		key.paths = gatherPaths(name, key.coordinate, maze)
		maze.keys[name] = key
	}

	for i, entrance := range maze.entrances {
		maze.entrances[i].paths = gatherPaths('@', entrance.coordinate, maze)
	}

	return maze, nil
}

type collectKeysState struct {
	n_workers int
	workers   [4]rune
	distance  int
	keys      int64
}

func makeInitialCollectKeysState(n_workers int) collectKeysState {
	var array [4]rune

	for i := 0; i < n_workers; i += 1 {
		array[i] = '@'
	}
	return collectKeysState{n_workers, array, 0, 0}
}

func (self *collectKeysState) traversePath(worker int, path path) {
	self.workers[worker] = path.to
	self.keys = self.keys | path.key
	self.distance += path.distance
}

func (self *collectKeysState) backtrack(worker int, path path) {
	self.workers[worker] = path.from
	self.keys = self.keys & ^path.key
	self.distance -= path.distance
}

type cache map[collectKeysState]int

func collectKeys(state collectKeysState, best int, maze *maze, cache cache) int {
	if state.keys == (1<<len(maze.keys))-1 {
		// We've picked up all keys
		cache[state] = state.distance
		return state.distance
	} else if best <= state.distance {
		// There is a better route
		cache[state] = 1<<63 - 1
		return best
	} else if score, ok := cache[state]; ok {
		// We've been here before
		return score
	}

	for i := 0; i < state.n_workers; i++ {
		worker := state.workers[i]

		var paths []path
		if '@' == worker {
			paths = maze.entrances[i].paths
		} else {
			paths = maze.keys[worker].paths
		}

		for _, path := range paths {
			if 0 < state.keys&path.key {
				// Skip if this key was already collected
				continue
			} else if path.door_bitmap != path.door_bitmap&state.keys {
				// Skip if the key is behind doors for which we don't have keys
				continue
			} else if path.key_bitmap != path.key_bitmap&state.keys {
				// Skip if the key is behind other keys that we haven't picked up yet.
				continue
			} else if best <= state.distance+path.distance {
				continue
			}

			state.traversePath(i, path)
			// if config.Render {
			// 	renderMaze(maze)
			// }
			score := collectKeys(state, best, maze, cache)
			state.backtrack(i, path)

			if score < best {
				best = score
			}
		}
	}
	cache[state] = best
	return best
}

func renderMaze(maze *maze) {
	builder := strings.Builder{}
	builder.WriteString("\033[H\033[2J")
	for y := 0; y < len(maze.grid); y++ {
		for x := 0; x < len(maze.grid[0]); x++ {
			switch ch := maze.grid[y][x]; {
			case 'a' <= ch && ch <= 'z':
				builder.WriteString("\033[1;31m")
				builder.WriteRune(ch)
				builder.WriteString("\033[0m")
			case 'A' <= ch && ch <= 'Z':
				builder.WriteString("\033[1;32m")
				builder.WriteRune(ch)
				builder.WriteString("\033[0m")
			default:
				builder.WriteRune(maze.grid[y][x])
			}
		}
		builder.WriteRune('\n')
	}
	fmt.Println(builder.String())
	time.Sleep(20 * time.Millisecond)
}

func collectKeysInMaze(maze *maze) int {
	cache := make(cache)

	var best int = 1<<63 - 1
	state := makeInitialCollectKeysState(len(maze.entrances))

	for i := 0; i < len(maze.entrances); i += 1 {
		for _, path := range maze.entrances[i].paths {
			if 0 < path.door_bitmap {
				continue
			}

			state.traversePath(i, path)
			score := collectKeys(state, best, maze, cache)
			state.backtrack(i, path)

			if score < best {
				best = score
			}
		}
	}

	return best
}

func Solve() error {
	var maze *maze
	var err error

	maze, err = readMaze("./input/18_part1.txt")
	if err != nil {
		return err
	}
	fmt.Println("Day 18, Part 1. Let a robot navigate a vault with keys and doors.")
	fmt.Println(" ", collectKeysInMaze(maze))

	maze, err = readMaze("./input/18_part2.txt")
	if err != nil {
		return err
	}
	fmt.Println("Day 18, Part 2. Let 4 robots navigate 4 vaults in parallel.")
	fmt.Println(" ", collectKeysInMaze(maze))

	return nil
}
