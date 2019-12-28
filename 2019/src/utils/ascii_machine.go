package utils

import "fmt"

type AsciiMachine struct {
	machine Machine
	Input   chan int64
	Output  chan int64
	Error   chan error
}

func MakeAsciiMachine(name string, program Program) AsciiMachine {
	ascii := AsciiMachine{}
	ascii.machine = MakeMachine(name, program)
	ascii.Input = ascii.machine.Input
	ascii.Output = ascii.machine.Output
	ascii.Error = ascii.machine.Error
	return ascii
}

func (self *AsciiMachine) Run() {
	self.machine.Run()
}

func (self *AsciiMachine) SetMemory(address uint64, value int64) {
	self.machine.SetMemory(address, value)
}

func (self *AsciiMachine) ReceiveUntil(until string) (string, error) {
	var received []rune

PromptLoop:
	for {
		select {
		case output := <-self.Output:
			received = append(received, rune(output))
			if len(until) <= len(received) && string(received[len(received)-len(until):]) == until {
				break PromptLoop
			}
		case err := <-self.Error:
			if err != nil {
				return "", err
			}
			return "", fmt.Errorf("Machine died while handling prompt")
		}
	}
	return string(received), nil
}

func (self *AsciiMachine) SendString(str string) error {
	var index int
AnswerLoop:
	for {
		select {
		case self.Input <- int64(str[index]):
			index += 1
			if index == len(str) {
				break AnswerLoop
			}
		case err := <-self.Error:
			if err != nil {
				return err
			}
			return fmt.Errorf("Machine died while answering prompt")
		}
	}
	return nil
}

func (self *AsciiMachine) ReceiveUntilTermination() (string, error) {
	var received []rune

	for {
		select {
		case output := <-self.Output:
			received = append(received, rune(output))
		case err := <-self.Error:
			if nil != err {
				return "", err
			} else {
				return string(received), nil
			}
		}
	}
}

func (self *AsciiMachine) ReceiveNRunes(n_runes int) (string, error) {
	var received []rune

	for len(received) < n_runes {
		select {
		case output := <-self.Output:
			received = append(received, rune(output))
		case err := <-self.Error:
			if nil != err {
				return "", err
			} else {
				return "", fmt.Errorf("Terminated before n bytes could be read")
			}
		}
	}
	return string(received), nil
}
