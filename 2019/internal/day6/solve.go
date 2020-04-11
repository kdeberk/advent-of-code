package day6

import (
	"fmt"
)

func part1(a_tree *tree) int {
	sum := 0
	for _, node := range a_tree.nodes {
		sum += node.depth
	}
	return sum
}

func part2(a_tree *tree) int {
	n_transfers := 0

	you, san := a_tree.nodes["YOU"].parent, a_tree.nodes["SAN"].parent
	for you.depth < san.depth {
		san = san.parent
		n_transfers += 1
	}
	for san.depth < you.depth {
		you = you.parent
		n_transfers += 1
	}
	for san != you {
		san, you = san.parent, you.parent
		n_transfers += 2
	}

	return n_transfers
}

func Solve() error {
	tree, err := readTree("./input/6.txt")
	if err != nil {
		return err
	}

	fmt.Println("Day 6, Part 1. Calculate total number of direct and indirect orbits of all objects in our map.")
	fmt.Println(" ", part1(&tree))
	fmt.Println("Day 6, Part 2. Calculate number of orbital transfers needed to reach Santa.")
	fmt.Println(" ", part2(&tree))
	return nil
}
