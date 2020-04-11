package utils

import "strings"

func TrimASCIIArt(s string) string {
	lines := []string{}

	for _, line := range strings.Split(s, "\n") {
		line = strings.TrimSpace(line)
		if 0 < len(line) {
			lines = append(lines, line)
		}
	}

	return strings.Join(lines, "\n")
}
