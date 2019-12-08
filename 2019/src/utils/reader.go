package utils

import (
	"bufio"
	"os"
	"strconv"
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

func ReadStrings(filename string, split_at func(rune) bool) ([]string, error) {
	scanner, err := makeScannerForFile(filename, split_at)
	if err != nil {
		return []string{}, err
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
		return []uint64{}, err
	}

	numbers := []uint64{}
	for scanner.Scan() {
		number, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return []uint64{}, err
		}
		numbers = append(numbers, uint64(number))
	}
	return numbers, nil
}

func ReadInt64s(filename string, split_at func(rune) bool) ([]int64, error) {
	scanner, err := makeScannerForFile(filename, split_at)
	if err != nil {
		return []int64{}, err
	}

	numbers := []int64{}
	for scanner.Scan() {
		number, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return []int64{}, err
		}
		numbers = append(numbers, int64(number))
	}
	return numbers, nil
}
