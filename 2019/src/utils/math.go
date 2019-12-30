package utils

func GCD(a, b int) int {
	for 0 != b {
		t := b
		b = a % b
		a = t
	}
	return a
}

func LCM(a, b int, rest ...int) int {
	result := a * b / GCD(a, b)

	for _, i := range rest {
		result = result * i / GCD(result, i)
	}
	return result
}

func AbsInt(x int) int {
	if x < 0 {
		return -x
	} else {
		return x
	}
}
