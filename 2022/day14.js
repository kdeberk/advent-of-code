const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day14.txt', 'utf8')

const paths = (input.lines()
                    .map(l => l.split(" -> ")
                               .map(xy => xy.split(",").map(x => x*1))))

const points = (paths.reduce((x, y) => x.concat(y)))
const minX = points.map(xy => xy[0]).min()
const maxX = points.map(xy => xy[0]).max()
const maxY = points.map(xy => xy[1]).max()

const START = 's'
const SAND = '0'
const AIR = '.'
const WALL = '#'
const PATH = '~'
const FALL = 'v'

class InfiniteFallGrid {
  constructor(minX, maxX, maxY) {
    this.minX = minX
    this.maxX = maxX
    this.maxY = maxY
    this.grid = (maxY+1).times(_ => new Array(maxX-minX+1).fill(AIR))
    this.grid[0][500-minX] = START
  }

  set(x, y, val) {
    if(this.maxY < y || x < this.minX || this.maxX < x) {
      return
    }
    this.grid[y][x-this.minX] = val
  }

  get(x, y) {
    if(this.maxY < y || x < this.minX || this.maxX < x) {
      return FALL
    }
    return this.grid[y][x-this.minX]
  }

  draw() {
    return this.grid.map(r => r.join('')).join('\n')
  }
}

class BottomedGrid {
  constructor(maxX, maxY) {
    this.maxX = maxX
    this.maxY = maxY
    this.grid = (maxY+1).times(_ => new Array(maxX+1).fill(AIR))
    this.grid[0][500] = START
    for(let x = 0; x < maxX; x++) {
      this.grid[maxY][x] = WALL
    }
  }

  get(x, y) {
    return this.grid[y][x]
  }

  set(x, y, val) {
    if(this.maxY < y || x < 0 || this.maxX < x) {
      throw("Out of bounds: " + x + "," + y)
    }
    this.grid[y][x] = val
  }
}

const sign = (a, b) => (a < b) ? 1 : (a == b) ? 0 : -1
const drawPaths = (paths, grid) => {
  paths.each(p => {
    for(idx = 0; idx < p.length-1; idx++) {
      const start = p[idx]
      const end = p[idx+1]
      const dx = sign(start[0], end[0])
      const dy = sign(start[1], end[1])

      for(let [x, y] = start; ; x += dx, y += dy) {
        grid.set(x, y, WALL)
        if(x == end[0] && y == end[1]) {
          break
        }
      }
    }
  })
  return grid
}

const dropUnit = (grid, drawFall = false) => {
  let [curX, curY] = [500, 0]

  if(grid.get(curX, curY) == SAND) {
    return false
  }

FallLoop:
  while (true) {
    for(let [nx, ny] of [[curX, curY+1], [curX-1, curY+1], [curX+1, curY+1]]) {
      switch (grid.get(nx, ny)) {
        case FALL:
          return false
        case AIR:
        case PATH:
          curX = nx
          curY = ny

          if(drawFall) {
            grid.set(curX, curY, PATH)
          }
          continue FallLoop
      }
    }
    grid.set(curX, curY, SAND)
    return true
  }
}

const dropUnits = (grid) => {
  for(let count = 0; ; count++) {
    if(!dropUnit(grid, true)) {
      return count
    }
  }
}

console.log("Part1: ", dropUnits(drawPaths(paths, new InfiniteFallGrid(minX, maxX, maxY))))
console.log("Part1: ", dropUnits(drawPaths(paths, new BottomedGrid(2000, maxY+2))))
