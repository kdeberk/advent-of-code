package utils

import "strings"

func Prefix(text string, prefix string) string {
	lines := []string{}

	for _, line := range strings.Split(text, "\n") {
		lines = append(lines, prefix+line)
	}

	return strings.Join(lines, "\n")
}
