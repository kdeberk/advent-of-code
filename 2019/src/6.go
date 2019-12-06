package main

import (
	"fmt"
	"strings"
	"utils"
)

const part1Answer = 251208
const part2Answer = 397

type Node struct {
	name     string
	parent   *Node
	children []*Node
	depth    uint64
}

func newNode(name string) *Node {
	return &Node{name, nil, []*Node{}, 0}
}

func (self *Node) setDepth(depth uint64) {
	self.depth = depth
	for _, child := range self.children {
		child.setDepth(depth + 1)
	}
}

type Tree struct {
	nodes map[string]*Node
}

func (self *Tree) addChild(parent_name string, child_name string) {
	parent, ok := self.nodes[parent_name]
	if !ok {
		parent = newNode(parent_name)
		self.nodes[parent_name] = parent
	}

	child, ok := self.nodes[child_name]
	if !ok {
		child = newNode(child_name)
		self.nodes[child_name] = child
	}
	parent.children = append(parent.children, child)
	child.parent = parent
}

func (self *Tree) calculateDepths() {
	self.nodes["COM"].setDepth(0)
}

func readTree(filename string) (Tree, error) {
	lines, err := utils.ReadStrings(filename, utils.IsNewline)
	if err != nil {
		return Tree{}, err
	}

	tree := Tree{map[string]*Node{}}
	for _, line := range lines {
		parts := strings.Split(line, ")")
		if 2 != len(parts) {
			return Tree{}, fmt.Errorf("Misformatted line: %s", line)
		}

		tree.addChild(parts[0], parts[1])
	}
	tree.calculateDepths()
	return tree, nil
}

func part1(tree *Tree) uint64 {
	sum := uint64(0)
	for _, node := range tree.nodes {
		sum += node.depth
	}
	return sum
}

func part2(tree *Tree) uint64 {
	n_transfers := uint64(0)

	you, san := tree.nodes["YOU"].parent, tree.nodes["SAN"].parent
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

func main() {
	tree, err := readTree("../data/6.txt")
	if err != nil {
		panic(err)
	}

	var answer uint64
	answer = part1(&tree)
	if part1Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part1Answer))
	}
	fmt.Printf("Part 1: %d\n", answer)

	answer = part2(&tree)
	if part2Answer != answer {
		panic(fmt.Sprintf("Part 1 has wrong answer %d (correct %d)", answer, part2Answer))
	}
	fmt.Printf("Part 2: %d\n", answer)
}
