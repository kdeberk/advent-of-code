package day6

import (
	"fmt"
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
	"strings"
)

type node struct {
	name     string
	parent   *node
	children []*node
	depth    int
}

func makeNode(name string) *node {
	return &node{name, nil, []*node{}, 0}
}

func (self *node) addChild(child *node) {
	child.parent = self
	self.children = append(self.children, child)
}

func (self *node) setDepth(depth int) {
	self.depth = depth
	for _, child := range self.children {
		child.setDepth(depth + 1)
	}
}

type tree struct {
	nodes map[string]*node
}

func (self *tree) getNode(name string) *node {
	node, ok := self.nodes[name]
	if !ok {
		node = makeNode(name)
		self.nodes[name] = node
	}
	return node
}

func readTree(filename string) (tree, error) {
	a_tree := tree{map[string]*node{}}

	lines, err := utils.ReadStrings(filename, utils.IsNewline)
	if err != nil {
		return a_tree, err
	}

	for _, line := range lines {
		parts := strings.Split(line, ")")

		if 2 != len(parts) {
			return a_tree, fmt.Errorf("Misformatted line: %s", line)
		}

		parent := a_tree.getNode(parts[0])
		parent.addChild(a_tree.getNode(parts[1]))
	}

	a_tree.getNode("COM").setDepth(0)
	return a_tree, nil
}
