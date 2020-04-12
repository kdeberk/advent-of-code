package day14

import (
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

type reaction struct {
	product  string
	quantity int
	required map[string]int
}

func makeReaction(product string, required int) reaction {
	return reaction{product, required, make(map[string]int, 0)}
}

type quantified_name struct {
	quantity int
	name     string
}

func parseLine(line string) ([]quantified_name, error) {
	r := regexp.MustCompile(`,\s|\s=>\s`)

	result := make([]quantified_name, 0)
	for _, item := range r.Split(line, -1) {
		split := strings.Split(item, " ")

		quantity, err := strconv.Atoi(split[0])
		if err != nil {
			return result, err
		}

		result = append(result, quantified_name{quantity, split[1]})
	}
	return result, nil
}

func readReactions(filename string) (map[string]reaction, error) {
	lines, err := utils.ReadLines(filename)
	if err != nil {
		return map[string]reaction{}, err
	}

	reactions := make(map[string]reaction, len(lines))
	for _, line := range lines {
		quantified, err := parseLine(line)
		if err != nil {
			return reactions, err
		}

		product := quantified[len(quantified)-1]
		required := quantified[0 : len(quantified)-1]
		reactions[product.name] = makeReaction(product.name, product.quantity)

		for _, resource := range required {
			reactions[product.name].required[resource.name] = resource.quantity
		}
	}

	return reactions, nil
}

func createDependencyHierarchy(reactions map[string]reaction) [][]string {
	deps := map[string]int{"FUEL": 0}
	queue := []string{"FUEL"}

	maxDepth := 0
	var current string
	for 0 < len(queue) {
		current, queue = queue[0], queue[1:]

		depth := deps[current]
		if maxDepth < depth {
			maxDepth = depth
		}

		for child, _ := range reactions[current].required {
			if child_depth, ok := deps[child]; ok && depth < child_depth {
				// If child already exists in tree and is deep enough, then skip
				continue
			}
			deps[child] = depth + 1
			queue = append(queue, child)
		}
	}

	levels := make([][]string, maxDepth+1)
	for level := 0; level <= maxDepth; level++ {
		levels[level] = []string{}
	}

	for resource, level := range deps {
		levels[level] = append(levels[level], resource)
	}

	return levels
}

func requiredOreForFuel(nFuel int, reactions map[string]reaction) int {
	hier := createDependencyHierarchy(reactions)

	needed := map[string]int{}
	for resource := range reactions {
		needed[resource] = 0
	}
	needed["FUEL"] = nFuel

	for _, level := range hier[:len(hier)-1] {
		for _, resource := range level {
			n_reactions := int(math.Ceil(float64(needed[resource]) / float64(reactions[resource].quantity)))

			for dep, amount := range reactions[resource].required {
				needed[dep] += n_reactions * amount
			}
		}
	}

	return needed["ORE"]
}

func part1(reactions map[string]reaction) int {
	return requiredOreForFuel(1, reactions)
}

func part2(reactions map[string]reaction) int {
	oneFuel := requiredOreForFuel(1, reactions)
	ratio := 10e12 / oneFuel

	// Don't ask me, I don't know how this works...
	return int(float64(ratio) * (float64(10e11) / float64(requiredOreForFuel(ratio, reactions))))
}

func Solve() error {
	reactions, err := readReactions("./input/14.txt")
	if err != nil {
		return err
	}

	fmt.Println("Day 14, Part 1. Calculate the minimum amount of ORE needed to produce 1 FUEL")
	fmt.Println(" ", part1(reactions))
	fmt.Println("Day 14, Part 2. Calculate the amount of FUEL that can be produced given 10**12 ORE")
	fmt.Println(" ", part2(reactions))

	return nil
}
