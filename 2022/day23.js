"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day23.txt', 'utf8')

const locateElves = (grid) => {
  const pos = {}
  for(let y = 0; y < grid.length; y++) {
    for(let x = 0; x < grid[0].length; x++) {
      if(grid[y][x] == '#') {
        pos[x+"-"+y] = [x, y]
      }
    }
  }
  return pos
}

const NW = [-1, -1]
const N  = [ 0, -1]
const NE = [ 1, -1]
const W  = [-1,  0]
const E  = [ 1,  0]
const SW = [-1,  1]
const S  = [ 0,  1]
const SE = [ 1,  1]

const hasNeighbors = ([x, y], elves) => {
  for(const [dx, dy] of [NW, N, NE, W, E, SW, S, SE]) {
    if(elves[(x+dx)+"-"+(y+dy)] != undefined) {
      return true
    }
  }
  return false
}

const propose = (elves, [x, y], directions) => {
  DirLoop:
  for(const dir of directions) {
    for(const [dx, dy] of dir) {
      if(elves[(x+dx)+"-"+(y+dy)] != undefined) {
        continue DirLoop
      }
    }
    const [dx, dy] = dir[0]
    return [x+dx, y+dy]
  }
}

const directions = [[N, NE, NW], [S, SE, SW], [W, NW, SW], [E, NE, SE]]

const round = (elves, i) => {
  const dir = directions.cycle(i)
  let moved = false

  const proposed = {}
  for(const k of Object.keys(elves)) {
    const [x, y] = elves[k]
    if(!hasNeighbors([x, y], elves)) {
      proposed[k] = [[x, y], [x, y]]
      continue // Don't move
    }

    const prop = propose(elves, [x, y], dir)
    if(prop == undefined) {
      proposed[k] = [[x, y], [x, y]]
      continue
    }

    moved = true
    const [nx, ny] = prop
    const l = nx+"-"+ny
    if(proposed[l] == undefined) {
      proposed[l] = [[nx, ny], [x, y]]
    } else {
      proposed[l].push([x, y])
    }
  }

  if(!moved) {
    return null
  }

  const newElves = {}
  for(const k of Object.keys(proposed)) {
    const v = proposed[k]
    if(v.length == 2) {
      newElves[k] = v[0]
    } else {
      for(let idx = 1; idx < v.length; idx++) {
        const [x, y] = v[idx]
        newElves[x+"-"+y] = v[idx]
      }
    }
  }

  return newElves
}

const countEmpty = (elves) => {
  const minX = Object.values(elves).map(([x, _]) => x).min()
  const maxX = Object.values(elves).map(([x, _]) => x).max()
  const minY = Object.values(elves).map(([_, y]) => y).min()
  const maxY = Object.values(elves).map(([_, y]) => y).max()

  return (maxX-minX+1)*(maxY-minY+1) - Object.keys(elves).length
}

const draw = (elves) => {
  const minX = Object.values(elves).map(([x, _]) => x).min()
  const maxX = Object.values(elves).map(([x, _]) => x).max()
  const minY = Object.values(elves).map(([_, y]) => y).min()
  const maxY = Object.values(elves).map(([_, y]) => y).max()

  let s = ''
  for(let y = minY - 2; y < maxY + 2; y++) {
    for(let x = minX - 2; x < maxX + 2; x++) {
      s += (elves[x+"-"+y] != undefined) ? '#' : '.'
    }
    s += '\n'
  }
  return s
}

const part1 = (elves) => {
  for(let idx = 0; idx < 10; idx++) {
    elves = round(elves, idx)
  }
  return countEmpty(elves)
}

const part2 = (elves) => {
  for(let idx = 0; ; idx++) {
    const next = round(elves, idx)
    if(next == null) {
      return idx + 1
    }
    elves = next
  }
}

let elves = locateElves(input.grid())

console.log("Part1:", part1(elves))
console.log("Part2:", part2(elves))
