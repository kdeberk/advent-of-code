package day14

import (
	"fmt"
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

func calculateRequiredOreForFuel(n_fuel int, reactions map[string]reaction) int {
	needed, leftover := make(map[string]int, 0), make(map[string]int, 0)
	ore_needed := 0

	needed["FUEL"] = n_fuel

	for 0 < len(needed) {
		for resource, n_needed := range needed {
			reaction := reactions[resource]

			// Check previous generated leftovers
			if n_needed < leftover[resource] {
				n_needed = 0
				leftover[resource] -= n_needed
			} else {
				n_needed -= leftover[resource]
				leftover[resource] = 0
			}

			// Calculate how many reactions we need to execute
			var n_reactions int
			if 0 == n_needed%reaction.quantity {
				n_reactions = n_needed / reaction.quantity
			} else {
				n_reactions = n_needed/reaction.quantity + 1
			}

			// Store leftover products
			leftover[reaction.product] += n_reactions*reaction.quantity - n_needed

			// Cascade requirements
			for required, quantity := range reaction.required {
				if "ORE" == required {
					ore_needed += n_reactions * quantity
				} else {
					needed[required] += n_reactions * quantity
				}
			}

			delete(needed, resource)
		}
	}

	return ore_needed
}

func part1(reactions map[string]reaction) int {
	return calculateRequiredOreForFuel(1, reactions)
}

func part2(reactions map[string]reaction) int {
	var n_fuel int
	for n_fuel = 1_800_000; ; n_fuel++ {
		n_ore := calculateRequiredOreForFuel(n_fuel, reactions)

		if 1_000_000_000_000 < n_ore {
			return n_fuel - 1
		}
	}
}

func Solve() error {
	reactions, err := readReactions("day14/14.txt")
	if err != nil {
		return err
	}

	fmt.Printf("Day 14, Part 1: %d\n", part1(reactions))
	fmt.Printf("Day 14, Part 2: %d\n", part2(reactions))

	return nil
}
