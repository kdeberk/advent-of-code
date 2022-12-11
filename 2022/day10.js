const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day10.txt', 'utf8')

const diff = (a, b) => Math.abs(a-b)
const instructions = input.lines()
                          .map(l => {
                            [ins, count] = l.split(" ")
                            if(count) {
                              return [ins, count*1]
                            }
                            return [ins]
                          })

class Machine {
  constructor() {
    this.x = 1
    this.cycle = 0
    this.strength = 0
    this.points = [20, 60, 100, 140, 180, 220, 260]
    this.crt = new CRT()
  }

  handle([ins, x]) {
    switch(ins) {
      case "noop":
        this.noop()
        break
      case "addx":
        this.addx(x)
    }
    return this
  }

  noop() {
    this.incCycle()
  }

  addx(x) {
    this.incCycle()
    this.incCycle()
    this.x += x
  }

  incCycle() {
    this.cycle++

    if(this.points[0] <= this.cycle) {
      this.strength += this.x * this.points[0]
      this.points.shift()
    }

    this.crt.drawSprite(this.x)
  }
}

const SCREEN_WIDTH = 40
const SCREEN_HEIGHT = 6

class CRT {
  constructor() {
    this.drawX = 0
    this.drawY = 0
    this.screen = (SCREEN_HEIGHT).times(_ =>  Array(SCREEN_WIDTH).fill(' '))
  }


  drawSprite(x) {
    if(diff(x, this.drawX) < 2) {
      this.screen[this.drawY][this.drawX] = '#'
    }
    this.incCycle()
  }

  incCycle() {
    this.drawX++
    if(this.drawX == SCREEN_WIDTH) {
      this.drawX = 0
      this.drawY++
    }
  }
}

const m = new Machine()
instructions.reduce((m, ins) => m.handle(ins), m)

console.log("Part1:", m.strength)
console.log("Part2:")
console.log(m.crt.screen.map(l => l.join('')).join("\n"))
