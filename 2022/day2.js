require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day2.txt', 'utf8')

const abc = {"A": "rock", "B": "paper", "C": "scissors"}
const xyz = {"X": "rock", "Y": "paper", "Z": "scissors"}

const scores = {
  "rock":     {"rock": 1+3, "paper": 2+6, "scissors": 3+0},
  "paper":    {"rock": 1+0, "paper": 2+3, "scissors": 3+6},
  "scissors": {"rock": 1+6, "paper": 2+0, "scissors": 3+3},
}

const response = {
  "X": {"rock": "scissors", "paper": "rock", "scissors": "paper"},
  "Y": {"rock": "rock", "paper": "paper", "scissors": "scissors"},
  "Z": {"rock": "paper", "paper": "scissors", "scissors": "rock"},
}

console.log("Part1:", input.
            split("\n").slice(0, -1).
            map(line => {
              const [a, b] = line.split(" ")
              return scores[abc[a]][xyz[b]]
            }).
            sum())

console.log("Part2:", input.
            split("\n").slice(0, -1).
            map(line => {
              const [a, b] = line.split(" ")
              const action = abc[a]
              const answer = response[b]
              return scores[action][answer[action]]
            }).
            sum())
