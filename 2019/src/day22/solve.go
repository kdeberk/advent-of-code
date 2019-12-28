package day22

import (
	"fmt"
	"math/big"
	"regexp"
	"strconv"
	"utils"
)

const (
	deal_into_new_stack int = iota
	cut                     = iota
	deal_with_increment     = iota
)

type step struct {
	kind  int
	value int
}

var rDealIntoNewStack = regexp.MustCompile(`deal into new stack`)
var rCut = regexp.MustCompile(`cut (-?\d+)`)
var rDealWithIncrement = regexp.MustCompile(`deal with increment (\d+)`)

func readSteps(filename string) ([]step, error) {
	lines, err := utils.ReadLines(filename)
	if err != nil {
		return []step{}, err
	}

	steps := []step{}
	for _, line := range lines {
		switch {
		case rDealIntoNewStack.MatchString(line):
			steps = append(steps, step{deal_into_new_stack, 0})
		case rCut.MatchString(line):
			i, _ := strconv.Atoi(rCut.FindStringSubmatch(line)[1])
			steps = append(steps, step{cut, i})
		case rDealWithIncrement.MatchString(line):
			i, _ := strconv.Atoi(rDealWithIncrement.FindStringSubmatch(line)[1])
			steps = append(steps, step{deal_with_increment, i})
		}
	}

	return steps, nil
}

func mod(a, b int) int {
	m := a % b
	if m < 0 {
		m += b
	}
	return m
}

func applyStepsKeepTrackOf(steps []step, card int, n_cards int) int {
	position := card

	for _, step := range steps {
		switch step.kind {
		case deal_into_new_stack:
			position = n_cards - position - 1
		case cut:
			position = mod(position-step.value, n_cards)
		case deal_with_increment:
			position = mod(position*step.value, n_cards)
		}
	}
	return position
}

func calcLinear(steps []step, exp, mod *big.Int) (*big.Int, *big.Int) {
	increment, offset := big.NewInt(1), big.NewInt(0)

	var tmp big.Int
	for _, step := range steps {
		tmp.SetInt64(int64(step.value))

		switch step.kind {
		case deal_into_new_stack:
			increment.Neg(increment)      // increment *= -1
			offset.Add(offset, increment) // offset += increment
		case cut:
			offset.Add(offset, tmp.Mul(increment, &tmp)) // offset += increment * step.value
		case deal_with_increment:
			increment.Mul(increment, tmp.Exp(&tmp, exp, mod)) // increment *= step.value^(n_cards - 2) mod n_cards
		}
	}
	return increment, offset
}

func part1(steps []step) int {
	return applyStepsKeepTrackOf(steps, 2019, 10_007)
}

func part2(steps []step) int {
	position := big.NewInt(2020)
	n_cards := big.NewInt(119_315_717_514_047)
	n_iterations := big.NewInt(101_741_582_076_661)

	// Most difficult one for me, had to check the subreddit for clues.
	// Three very important themes are:
	// - converting the steps to a linear expression position*increment + offset
	// - calculating the modular inverse of a multiplication (e.g. obtaining a from a*b (mod c) where b and c are known):
	//     position = position*step.value*inv(step.value) where inv(a) = a^(n_cards-2) (mod n_cards)
	//     this follows from Euler's theorem: a^phi(m)-1 ≡ a^-1 and when m is prime: a^m-2 ≡ a^-1
	// - using the geometric series formula to calculate the offset after repeated iterations of the shuffle.

	var exp, increment, offset, pow, inv, answer big.Int

	exp.Sub(n_cards, big.NewInt(2)) // exp := n_cards - 2
	increment_mul, offset_diff := calcLinear(steps, &exp, n_cards)
	increment.Exp(increment_mul, n_iterations, n_cards) // increment := increment_mul^n_iterations mod n_cards
	pow.Exp(increment_mul, n_iterations, n_cards)       // pow := 1 - (increment_mul^n_iterations mod n_cards)
	pow.Sub(big.NewInt(1), &pow)                        //
	inv.Sub(big.NewInt(1), increment_mul)               // inv := inv(1 - increment_mul)
	inv.Exp(&inv, &exp, n_cards)                        //
	offset.Mul(offset_diff, &pow)                       // offset := offset_diff * (1 - (increment_mul^n_iterations mod n_cards)) * inv(1 - increment_mul)
	offset.Mul(&offset, &inv)                           //  This is the formula of the geometric series a + ar + ar^2 + .. + ar^(n-1)= a + (1 - r^n) * inv(1 - r)

	answer.Mul(position, &increment) // answer := increment * n + offset (mod n_cards)
	answer.Add(&answer, &offset)
	answer.Mod(&answer, n_cards)

	return int(answer.Int64())
}

func Solve() error {
	steps, err := readSteps("day22/22.txt")
	if err != nil {
		return err
	}

	fmt.Println("Day 22, Part 1:", part1(steps))
	fmt.Println("Day 22, Part 2:", part2(steps))
	return nil
}
