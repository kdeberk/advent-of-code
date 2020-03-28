package day23

import (
	"fmt"
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

// TODO: clean up, create network object with execution steps

func part1(program utils.Program) (int, error) {
	machines := make([]utils.Machine, 50)
	queues := make([][]int64, 50)

	for i := range machines {
		machines[i] = utils.MakeMachine("day23", program)
		go machines[i].Run()
		machines[i].Input <- int64(i)

		queues[i] = []int64{}
	}

	var answer int64
	n_running_machines := len(machines)

	for 0 < n_running_machines {
	MachineLoop:
		for i := range machines {
			machine := machines[i]
			queue := queues[i]

			if machine.HasTerminated() {
				continue MachineLoop
			}

			var input int64 = -1
			if 0 < len(queue) {
				input = queue[0]
			}

			select {
			case machine.Input <- input:
				if -1 != input {
					machine.Input <- queue[1]
					queues[i] = queue[2:]
				}
			case address := <-machine.Output:
				x := <-machine.Output
				y := <-machine.Output
				if 255 == address {
					answer = y
					for i := range machines {
						machines[i].Halt()
					}
				} else {
					queues[address] = append(queues[address], x, y)
				}
			case err := <-machine.Error:
				if err != nil {
					return 0, err
				} else {
					n_running_machines -= 1
				}
			default:
			}
		}
	}

	return int(answer), nil
}

func part2(program utils.Program) (int, error) {
	machines := make([]utils.Machine, 50)
	queues := make([][]int64, 50)
	nat_queue := []int64{}

	for i := range machines {
		machines[i] = utils.MakeMachine("day23", program)
		go machines[i].Run()
		machines[i].Input <- int64(i)

		queues[i] = []int64{}
	}

	var answer int64
	n_running_machines := len(machines)

	for 0 < n_running_machines {
		idle := true

	MachineLoop:
		for i := range machines {
			machine := machines[i]
			queue := queues[i]

			if machine.HasTerminated() {
				continue MachineLoop
			}

			var input int64 = -1
			if 0 < len(queue) {
				input = queue[0]
			}

			select {
			case machine.Input <- input:
				if -1 != input {
					machine.Input <- queue[1]
					queues[i] = queue[2:]
					idle = false
				}
			case address := <-machine.Output:
				x := <-machine.Output
				y := <-machine.Output
				if 255 == address {
					nat_queue = []int64{x, y}
				} else {
					queues[address] = append(queues[address], x, y)
				}
				idle = false
			case err := <-machine.Error:
				if err != nil {
					return 0, err
				} else {
					n_running_machines -= 1
				}
			}
		}

		for _, queue := range queues {
			if 0 < len(queue) {
				idle = false
			}
		}

		if idle && 0 < len(nat_queue) {
			if answer == nat_queue[1] {
				for i := range machines {
					machines[i].Halt()
				}
			} else {
				queues[0] = append(queues[0], nat_queue[0], nat_queue[1])
				answer = nat_queue[1]
			}
		}
	}

	return int(answer), nil
}

func Solve() error {
	program, err := utils.ReadProgram("day23/23.txt")
	if err != nil {
		return err
	}

	var answer int
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 23, Part 1:", answer)

	answer, err = part2(program)
	if err != nil {
		return err
	}
	fmt.Println("Day 23, Part 2:", answer)

	return nil
}
