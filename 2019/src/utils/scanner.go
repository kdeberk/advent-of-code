package utils

import (
	"bufio"
	"os"
	"unicode/utf8"
)

func makeScannerForFile(filename string, split_at func(rune) bool) (*bufio.Scanner, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	reader := bufio.NewReader(file)
	scanner := bufio.NewScanner(reader)
	scanner.Split(makeWordScanner(split_at))

	return scanner, nil
}

func makeWordScanner(test func(rune) bool) func([]byte, bool) (int, []byte, error) {
	// Generalized version of bufio.ScanWords
	return func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		// Skip leading spaces.
		start := 0
		for width := 0; start < len(data); start += width {
			var r rune
			r, width = utf8.DecodeRune(data[start:])
			if !test(r) {
				break
			}
		}
		// Scan until space, marking end of word.
		for width, i := 0, start; i < len(data); i += width {
			var r rune
			r, width = utf8.DecodeRune(data[i:])
			if test(r) {
				return i + width, data[start:i], nil
			}
		}
		// If we're at EOF, we have a final, non-empty, non-terminated word. Return it.
		if atEOF && len(data) > start {
			return len(data), data[start:], nil
		}
		// Request more data.
		return start, nil, nil
	}
}

func IsNewline(r rune) bool {
	return '\n' == r
}

func IsWhiteSpaceOrComma(r rune) bool {
	switch r {
	case ',', '\n', ' ':
		return true
	}
	return false
}
