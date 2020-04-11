package day13

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

type coordinate struct {
	x int64
	y int64
}

type tile int64

const (
	empty  tile = 0
	wall        = 1
	block       = 2
	paddle      = 3
	ball        = 4
)

const (
	readX    int = 0
	readY        = 1
	readKind     = 2
)

type game struct {
	computer utils.Machine
	paddle   coordinate
	ball     coordinate
	board    [][]tile
	score    int64
	joystick chan int64
	error    chan error
	render   chan bool
	rendered chan bool
}

const (
	left  int64 = -1
	stay        = 0
	right       = 1
)

func makeGame(program utils.Program) game {
	computer := utils.MakeMachine("day13", program)

	board := make([][]tile, 50)
	for i := range board {
		board[i] = make([]tile, 50)
	}

	return game{
		computer,
		coordinate{},
		coordinate{},
		board,
		0,
		computer.Input,
		computer.Error,
		make(chan bool),
		make(chan bool),
	}
}

func (self *game) Start(coins int64) {
	self.computer.SetMemory(0, coins)
	go self.computer.Run()

	var x, y int64
	var drawState int = readX

	for {
		select {
		case output := <-self.computer.Output:
			switch drawState {
			case readX:
				x = output
				drawState = readY
			case readY:
				y = output
				drawState = readKind
			case readKind:
				if -1 == x && 0 == y {
					self.score = output
				} else {
					thing := tile(output)
					switch thing {
					case ball:
						self.ball = coordinate{x, y}
					case paddle:
						self.paddle = coordinate{x, y}
					}

					self.board[y][x] = thing
					self.render <- true
					<-self.rendered
				}
				drawState = readX
			}
		}
	}
}

func part1(program utils.Program) (int, error) {
	game := makeGame(program)

	go game.Start(1)

GameLoop:
	for {
		select {
		case <-game.render:
			game.rendered <- true
		case err := <-game.error:
			if err != nil {
				return 0, err
			} else {
				break GameLoop
			}
		}
	}

	var count int
	for y := range game.board {
		for x := range game.board[y] {
			if game.board[y][x] == block {
				count += 1
			}
		}
	}

	return count, nil
}

func renderBoard(board [][]tile) {
	print("\033[H\033[2J")

	builder := strings.Builder{}
	for y := 0; y < len(board); y++ {
		for x := 0; x < len(board[y]); x++ {
			switch board[y][x] {
			case empty:
				builder.WriteRune(' ')
			case wall:
				builder.WriteRune('#')
			case paddle:
				builder.WriteRune('=')
			case block:
				builder.WriteRune('x')
			case ball:
				builder.WriteRune('o')
			}
		}
		builder.WriteRune('\n')
	}

	fmt.Println(builder.String())
}

func movePad(game game) int64 {
	if game.paddle.x < game.ball.x {
		return right
	} else if game.ball.x < game.paddle.x {
		return left
	}
	return stay
}

func part2(program utils.Program, render bool) (int, error) {
	game := makeGame(program)
	go game.Start(2)

	ball := game.ball

GameLoop:
	for {
		select {
		case game.joystick <- movePad(game):
		case <-game.render:
			if ball != game.ball {
				if true == render {
					renderBoard(game.board)
					time.Sleep(10 * time.Millisecond)
				}
				ball = game.ball
			}
			game.rendered <- true
		case err := <-game.error:
			if err != nil {
				return 0, err
			} else {
				break GameLoop
			}
		default:
		}
	}

	return int(game.score), nil
}

func Solve() error {
	program, err := utils.ReadProgram("./input/13.txt")
	if err != nil {
		return err
	}

	var answer int
	answer, err = part1(program)
	if err != nil {
		return err
	}
	fmt.Printf("Day 13, Part 1: %d\n", answer)

	answer, err = part2(program, "1" == os.Getenv("AOC_RENDER"))
	if err != nil {
		return err
	}
	fmt.Printf("Day 13, Part 2: %d\n", answer)
	return nil
}
