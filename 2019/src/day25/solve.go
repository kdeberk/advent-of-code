package day25

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"utils"
)

func readPrompt(machine *utils.Machine) (string, error) {
	prompt := ""

	builder := strings.Builder{}
PromptLoop:
	for {
		select {
		case err := <-machine.Error:
			if err != nil {
				return "", err
			} else {
				return prompt, fmt.Errorf("Machine unexpectedly terminated")
			}
		case input := <-machine.Output:
			r := rune(input)
			builder.WriteRune(r)

			if '\n' == r {
				line := builder.String()
				builder.Reset()
				prompt += line
				if "Command?\n" == line {
					break PromptLoop
				}
			}
		}
	}
	return prompt, nil
}

func answerPrompt(ch chan int64, answer string) {
	for _, r := range answer {
		ch <- int64(r)
	}
}

func Solve() error {
	program, err := utils.ReadProgram("day25/25.txt")
	if err != nil {
		return err
	}

	droid := utils.MakeAsciiMachine("day25", program)
	go droid.Run()

	stdin := bufio.NewReader(os.Stdin)

	for {
		prompt, err := droid.ReceiveUntil("Command?")
		fmt.Println(prompt)
		if err != nil {
			return err
		}

		fmt.Print("> ")
		answer, err := stdin.ReadString('\n')
		if err != nil {
			return err
		}
		droid.SendString(answer)
	}

	return nil
}
