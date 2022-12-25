"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day22.txt', 'utf8')

let [map, path] = input.split("\n\n")
map = map.grid()
path = [...path.matchAll(/(\d+)([LR])?/g)].map(m => [m[1] * 1, m[2]])

const BOUNDARY = ' '
const OPEN = '.'
const WALL = '#'

const findStart = (map) => {
  for(let x = 0; x < map[0].length; x++) {
    if(map[0][x] != BOUNDARY) {
      return [x, 0]
    }
  }
}

const turn = (face, change) => {
  switch(face) {
    case RIGHT:
      return change == 'L' ? UP : DOWN
    case LEFT:
      return change == 'L' ? DOWN : UP
    case UP:
      return change == 'L' ? LEFT : RIGHT
    case DOWN:
      return change == 'L' ? RIGHT : LEFT
  }
}

const mod = (m, n) => {
  while(m < 0) {
    m += n
  }
  return m % n
}

const RIGHT = 0
const DOWN = 1
const LEFT = 2
const UP = 3

const step = (map, [x, y], face) => {
  switch(face) {
    case RIGHT:
      return [mod(x+1, map[0].length), y]
    case LEFT:
      return [mod(x-1, map[0].length), y]
    case UP:
      return [x, mod(y-1, map.length)]
    case DOWN:
      return [x, mod(y+1, map.length)]
    default:
      throw("no face " + face)
  }
}

const lookup = (map, [x, y]) => {
  if(y < 0 || map.length <= y || x < 0 || map[y].length <= x) {
    return BOUNDARY
  }
  switch(map[y][x]) {
    case '^':
    case 'v':
    case '>':
    case '<':
      return OPEN
    default:
      return map[y][x]
  }
}

const walkPath = (map, path, jumpBoundary) => {
  let [x, y] = findStart(map)
  let face = RIGHT
  let j = 0
  for(const [n, t] of path) {
    StepLoop:
    for(let i = 0; i < n; i++) {
      switch(face) {
        case LEFT:
          map[y][x] = '<'
          break
        case RIGHT:
          map[y][x] = '>'
          break
        case UP:
          map[y][x] = '^'
          break
        case DOWN:
          map[y][x] = 'v'
          break
      }

      let [nx, ny] = step(map, [x, y], face)
      let nface = null

      switch(lookup(map, [nx, ny])) {
        case BOUNDARY: {
          [[nx, ny], nface] = jumpBoundary([x, y], [nx, ny], face)
          if(lookup(map, [nx, ny]) == OPEN) {
            [x, y, face] = [nx, ny, nface]
          }
          break
        }
        case OPEN:
          [x, y] = [nx, ny]
          break
        case WALL:
          break StepLoop
        default:
          throw("bad value " + lookup(map, [nx, ny]))
      }
    }

    if(t != undefined) {
      face = turn(face, t)
    }
  }
  return 1000*(y+1) + 4*(x+1) + face
}

console.log("Part1:", walkPath(map, path, (_, [x, y], face) => {
  while(lookup(map, [x, y]) == BOUNDARY) {
    [x, y] = step(map, [x, y], face)
  }
  return [[x, y], face]
}))

const wrap = ([x, y], face) => {
  switch(face) {
    case RIGHT:
      x = mod(x+1, 50)
      break
    case LEFT:
      x = mod(x-1, 50)
      break
    case UP:
      y = mod(y-1, 50)
      break
    case DOWN:
      y = mod(y+1, 50)
      break
  }
  return [[x, y], face]
}
const invertY = ([x, y], face) => [[x, 49 - y], [LEFT, '', RIGHT, ''][face]]
const swapXY = ([x, y], face) => [[y, x], [UP, LEFT, DOWN, RIGHT][face]]
const getSide = ([x, y]) => {
  for(const side of Object.keys(sides)) {
    const [sx, sy] = sides[side]
    const [dx, dy] = [x-sx, y-sy]
    if(0 <= dx && dx < 50 && 0 <= dy && dy < 50) {
      return side
    }
  }
}

// Manually constructed from challenge, not sure how to generate from input.
const sideSize = 50
const sides = {'1': [50, 0],
               '2': [100, 0],
               '3': [50, 50],
               '4': [50, 100],
               '5': [0, 100],
               '6': [0, 150]
              }
const transforms = {'1': [['2', wrap], ['3', wrap], ['5', invertY], ['6', swapXY]],
                    '2': [['4', invertY], ['3', swapXY], ['1', wrap], ['6', wrap]],
                    '3': [['2', swapXY], ['4', wrap], ['5', swapXY], ['1', wrap]],
                    '4': [['2', invertY] , ['6', swapXY], ['5', wrap], ['3', wrap]],
                    '5': [['4', wrap], ['6', wrap], ['1', invertY], ['3', swapXY]],
                    '6': [['4', swapXY], ['2', wrap], ['1', swapXY], ['5', wrap]],
                   }

console.log("Part2:", walkPath(map, path, ([ox, oy], _, face) => {
  const oSide = getSide([ox, oy])
  const [nSide, transform] = transforms[oSide][face]

  const [sox, soy] = sides[oSide]
  const [snx, sny] = sides[nSide]
  const [[nx, ny], nFace] = transform([ox-sox, oy-soy], face)
  return [[snx+nx, sny+ny], nFace]
}))
