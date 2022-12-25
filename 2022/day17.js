"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day17.txt', 'utf8')

const AIR = '.'
const ROCK = '#'
const FALLING = '@'
const SHAPES = [
  ['@@@@'],
  ['.@.', '@@@', '.@.'],
  ['..@', '..@', '@@@'],
  ['@', '@', '@', '@'],
  ['@@', '@@'],
]

class Grid {
  constructor() {
    this.rows = []
    this.height = 0
    this.nPlacedRocks = 0
  }

  placeRock(shape) {
    this.nPlacedRocks++
    (3).times(_ => {
      this.rows.push((7).times(_ => AIR))
    })

    shape.slice().reverse().each (row => {
      this.rows.push([AIR, AIR].concat(...row)
                               .concat(new Array(7 - 2 - row.length).fill(AIR)))
    })

    this.height += 3 + shape.length
  }

  canMove(to) {
    for(let y = 0; y < this.rows.length; y++) {
      for(let x = 0; x < 7; x++) {
        if(this.rows[y][x] != FALLING) {
          continue
        }

        const [fx, fy] = to(x, y)
        if(fy < 0 || this.rows.length <= fy || fx < 0 || 7 <= fx || this.rows[fy][fx] == ROCK) {
          return false
        }
      }
    }
    return true
  }

  move(to) {
    for(let y = 0; y < this.rows.length; y++) {
      for(let x = 0; x < 7; x++) {
        if(this.rows[y][x] != FALLING) {
          continue
        }

        const [fx, fy] = to(x, y)
        if(fy < 0 || fx < 0 || 7 <= fx) {
          continue
        }

        this.rows[y][x] = AIR
        this.rows[fy][fx] = FALLING
      }
    }
  }

  moveDown() {
    const down = (x, y) => [x, y-1]
    if(!this.canMove(down)) {
      return false
    }
    this.move(down)
    return true
  }

  moveLeft() {
    const left = (x, y) => [x-1, y]
    if(!this.canMove(left)) {
      return false
    }
    this.move(left)
    return true
  }

  moveRight() {
    const right = (x, y) => [x+1, y]
    if(!this.canMove(right)) {
      return false
    }

    for(let y = 0; y < this.rows.length; y++) {
      for(let x = 6; 0 <= x; x--) {
        if(this.rows[y][x] != FALLING) {
          continue
        }

        const [fx, fy] = right(x, y)
        if(fy < 0 || fx < 0 || 7 <= fx) {
          continue
        }

        this.rows[y][x] = AIR
        this.rows[fy][fx] = FALLING
      }
    }
    return true
  }

  fix() {
    this.trimTop()
    this.trimBottom()
    this.rows = this.rows.map(r => r.map(c => c == FALLING ? ROCK : c))
  }

  draw() {
    return this.rows.map(r => r.join('')).reverse().join('\n')
  }

  trimTop() {
    while(true) {
      let emptyTop = true
      for(let x = 0; x < 7; x++) {
        if(this.rows[this.rows.length-1][x] != AIR) {
          emptyTop = false
        }
      }
      if(!emptyTop) {
        break
      }
      this.rows.pop()
      this.height--
    }
  }

  trimBottom() {
    const deepest = {}
    const q = []
    for(let x = 0; x < 7; x++) {
      q.push([x, this.rows.length-1])
    }

    const seen = {}
    while(0 < q.length) {
      const [x,y] = q.shift()
      const k = x+"-"+y
      if(seen[k]) {
        continue
      }
      seen[k] = true
      if(deepest[x] == undefined || y < deepest[x]) {
        deepest[x] = y
      }
      for(let [dx, dy] of [[x+1, y], [x-1, y], [x, y-1]]) {
        if(dx < 0 || 7 <= dx || dy < 0) {
          continue
        }
        if(this.rows[dy][dx] == ROCK) {
          continue
        }
        q.push([dx, dy])
      }
    }

    const min = Object.values(deepest).min()
    if(0 < min) {
      this.rows = this.rows.slice(min)
    }
  }
}

const playTetris = (shapes, pattern, done) => {
  let shapeIdx = 0
  let jetIdx = 0

  const g = new Grid()
  g.placeRock(shapes[shapeIdx])

  while(true) {
    switch(pattern[jetIdx]) {
      case '<':
        g.moveLeft()
        break
      case '>':
        g.moveRight()
        break
      default:
        throw("Unknown!" + pattern[jetIdx])
    }

    if(!g.moveDown()) {
      g.fix()

      if(done(g, shapeIdx, jetIdx)) {
        return g.height
      }

      shapeIdx = (shapeIdx + 1) % shapes.length
      g.placeRock(shapes[shapeIdx])
    }

    jetIdx = (jetIdx + 1) % pattern.length
  }
}

const pattern = input.chars().slice(0, -1)
console.log("Part1:", playTetris(SHAPES, pattern, (grid) => grid.nPlacedRocks == 2022))

const part2 = (shapes, pattern) => {
  const SEARCHING = 0
  const WRAPPINGUP = 1
  const TOTAL_ROCKS = 1_000_000_000_000

  const seen = {}

  let state = SEARCHING
  return playTetris(shapes, pattern, (grid, patternIdx, jetIdx) => {
    switch(state) {
      case SEARCHING:
        const k = patternIdx+"-"+jetIdx+"-"+grid.rows.map(r => r.join('')).join('')

        if(seen[k] == undefined) {
          seen[k] = [grid.height, grid.nPlacedRocks]
          return false
        }

        const [prevHeight, prevPlaced] = seen[k]
        const diffHeight = grid.height-prevHeight
        const diffPlaced = grid.nPlacedRocks-prevPlaced

        const nRounds = Math.floor(TOTAL_ROCKS / diffPlaced)
        grid.nPlacedRocks += (nRounds-2)*diffPlaced
        grid.height += (nRounds-2)*diffHeight
        state = WRAPPINGUP
        return false
      case WRAPPINGUP:
        return TOTAL_ROCKS == grid.nPlacedRocks
    }
  })
}
console.log("Part2:", part2(SHAPES, pattern))
