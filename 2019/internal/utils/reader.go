package utils

import (
	"bufio"
	"io"
	"os"
	"strconv"
	"strings"
)

func ReadSingleLine(filename string) (string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return "", err
	}

	line, err := bufio.NewReader(file).ReadString('\n')
	if err != nil {
		return "", err
	}

	return line[:len(line)-1], nil
}

func ReadLines(filename string) ([]string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	lines := []string{}
	reader := bufio.NewReader(file)
ReadLinesLoop:
	for {
		line, err := reader.ReadString('\n')
		if err == io.EOF {
			break ReadLinesLoop
		} else if err != nil {
			return nil, err
		}
		lines = append(lines, strings.Trim(line, "\n"))
	}

	return lines, nil
}

func ReadStrings(filename string, split_at func(rune) bool) ([]string, error) {
	scanner, err := makeScannerForFile(filename, split_at)
	if err != nil {
		return nil, err
	}

	strings := []string{}
	for scanner.Scan() {
		strings = append(strings, scanner.Text())
	}
	return strings, nil
}

func ReadUint64s(filename string, split_at func(rune) bool) ([]uint64, error) {
	scanner, err := makeScannerForFile(filename, split_at)
	if err != nil {
		return nil, err
	}

	numbers := []uint64{}
	for scanner.Scan() {
		number, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		numbers = append(numbers, uint64(number))
	}
	return numbers, nil
}

func ReadInt64s(filename string, split_at func(rune) bool) ([]int64, error) {
	scanner, err := makeScannerForFile(filename, split_at)
	if err != nil {
		return nil, err
	}

	numbers := []int64{}
	for scanner.Scan() {
		number, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		numbers = append(numbers, int64(number))
	}
	return numbers, nil
}

func ReadInts(filename string, split_at func(rune) bool) ([]int, error) {
	scanner, err := makeScannerForFile(filename, split_at)
	if err != nil {
		return nil, err
	}

	numbers := []int{}
	for scanner.Scan() {
		number, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		} else {
			numbers = append(numbers, int(number))
		}
	}
	return numbers, nil
}
