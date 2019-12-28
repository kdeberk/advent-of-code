package day24

import (
	"fmt"
	"utils"
)

func readGrid(filename string) (int32, error) {
	lines, err := utils.ReadLines(filename)
	if err != nil {
		return 0, err
	}

	result := int32(0)
	for i := 0; i < 25; i += 1 {
		if '#' == lines[i/5][i%5] {
			result |= 1 << i
		}
	}
	return result, nil
}

func part1(grid int32) int32 {
	seen := make(map[int32]bool)

	for {
		next := int32(0)

		for cell := 0; cell < 25; cell += 1 {
			alive := 0 < grid&(1<<cell)
			count := int32(0)

			if 0 < cell%5 { // check left
				count += 1 & (grid >> (cell - 1))
			}
			if cell%5 < 4 {
				count += 1 & (grid >> (cell + 1)) // check right
			}
			if 4 < cell {
				count += 1 & (grid >> (cell - 5)) // check above
			}
			if cell < 20 {
				count += 1 & (grid >> (cell + 5)) // check bottom
			}

			if alive && 1 == count {
				next |= 1 << cell
			} else if !alive && (1 == count || 2 == count) {
				next |= 1 << cell
			}
		}

		if _, ok := seen[next]; ok {
			return next
		}
		seen[next] = true
		grid = next
	}

	return 0
}

func part2(grid int32) int32 {
	grids := []int32{grid}

	for minute := 0; minute < 200; minute += 1 {
		next_grids := []int32{}

		for index := -1; index < len(grids)+1; index += 1 {
			var outer, center, inner int32

			if 0 < index {
				outer = grids[index-1]
			}
			if 0 <= index && index < len(grids) {
				center = grids[index]
			}
			if index < len(grids)-1 {
				inner = grids[index+1]
			}

			next := int32(0)
			for cell := 0; cell < 25; cell += 1 {
				alive := 0 < center&(1<<cell)
				count := int32(0)

				if 12 == cell {
					continue
				}

				switch { // count bug on left
				case 0 == cell%5: // left side
					count += 1 & (outer >> 11)
				case 13 == cell: // directly right of inner
					count += 1 & (inner >> 4)
					count += 1 & (inner >> 9)
					count += 1 & (inner >> 14)
					count += 1 & (inner >> 19)
					count += 1 & (inner >> 24)
				default:
					count += 1 & (center >> (cell - 1))
				}

				switch { // count bug on right
				case 4 == cell%5: // right side
					count += 1 & (outer >> 13)
				case 11 == cell: // directly left of inner
					count += 1 & (inner >> 0)
					count += 1 & (inner >> 5)
					count += 1 & (inner >> 10)
					count += 1 & (inner >> 15)
					count += 1 & (inner >> 20)
				default:
					count += 1 & (center >> (cell + 1))
				}

				switch { // count bug above
				case cell < 5: // top side
					count += 1 & (outer >> 7)
				case 17 == cell: // directly beneath inner
					count += 1 & (inner >> 20)
					count += 1 & (inner >> 21)
					count += 1 & (inner >> 22)
					count += 1 & (inner >> 23)
					count += 1 & (inner >> 24)
				default:
					count += 1 & (center >> (cell - 5))
				}

				switch { // count bug beneath
				case 20 <= cell: // bottom side
					count += 1 & (outer >> 17)
				case 7 == cell: // directly above inner
					count += 1 & (inner >> 0)
					count += 1 & (inner >> 1)
					count += 1 & (inner >> 2)
					count += 1 & (inner >> 3)
					count += 1 & (inner >> 4)
				default:
					count += 1 & (center >> (cell + 5))
				}

				if alive && 1 == count {
					next |= 1 << cell
				} else if !alive && (1 == count || 2 == count) {
					next |= 1 << cell
				}
			}
			next_grids = append(next_grids, next)
		}
		grids = next_grids
	}

	count := int32(0)
	for _, grid := range grids {
		for 0 < grid {
			count += 1 & grid
			grid >>= 1
		}
	}

	return count
}

func Solve() error {
	grid, err := readGrid("day24/24.txt")
	if err != nil {
		return err
	}

	fmt.Println("Day 24, Part 1:", part1(grid))
	fmt.Println("Day 24, Part 2:", part2(grid))

	return nil
}
