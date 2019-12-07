package utils

func GeneratePermutations(ints []int64) <-chan []int64 {
	channel := make(chan []int64)
	go func() {
		defer close(channel)

		stack := make([]int, len(ints))
		output := make([]int64, len(ints))

		copy(output, ints)
		channel <- output

		for i := 0; i < len(ints); {
			if stack[i] < i {
				if 0 == i%2 {
					ints[0], ints[i] = ints[i], ints[0]
				} else {
					ints[stack[i]], ints[i] = ints[i], ints[stack[i]]
				}

				output = make([]int64, len(ints))
				copy(output, ints)
				channel <- output

				stack[i] += 1
				i = 0
			} else {
				stack[i] = 0
				i += 1
			}
		}
	}()
	return channel
}
