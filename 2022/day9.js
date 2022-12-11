const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day9.txt', 'utf8')

const diff = (a, b) => Math.abs(a-b)
const sign = a => (0 < a ? 1 : a == 0 ? 0 : -1)

class Knot {
  constructor(x, y, child) {
    this.x = x
    this.y = y
    this.child = child
  }

  move(dir) {
    switch(dir) {
      case 'U':
        this.y++
        break
      case 'D':
        this.y--
        break
      case 'L':
        this.x--
        break
      case 'R':
        this.x++
    }

    if(this.child !== null) {
      this.child.follow(this)
    }
  }

  follow(parent) {
    const dX = diff(parent.x, this.x)
    const dY = diff(parent.y, this.y)
    if(dX+dY < 2) {
      return // Either overlapping or touching
    } else if(dX == 1 && dY == 1) {
      return // Touching diagonally
    }

    if(dX != 0) { this.x += sign(parent.x-this.x) }
    if(dY != 0) { this.y += sign(parent.y-this.y) }

    if(this.child !== null) {
      this.child.follow(this)
    }
  }
}

const makeRope = n => n.reduce((parent, _) => new Knot(0, 0, parent), null)

const moveRope = (rope, moves) => {
  let last = rope
  for (; last.child !== null; last = last.child) {}

  const seen = new Map()
  for(let i = 0; i < moves.length; i++) {
    [dir, count] = moves[i]
    count.times(_ => {
      rope.move(dir)
      seen.set(last.x+"-"+last.y)
    })
  }
  return Array.from(seen.values()).length
}

const moves = input.lines()
                   .map(s => {
                     const [dir, count] = s.split(" ")
                     return [dir, count*1]
                   })

console.log("Part1:", moveRope(makeRope(2), moves))
console.log("Part2:", moveRope(makeRope(10), moves))
