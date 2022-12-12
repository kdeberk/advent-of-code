const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day12.txt', 'utf8')
const grid = input.lines().map(l => l.split(''))

const convert = (grid) => {
  const alphabet = "abcdefghijklmnopqrstuvwxyz"
  const codes = {'S': 0, 'E': 25}
  const a = 'a'.charCodeAt(0)
  for(let idx = 0; idx < alphabet.length; idx++) {
    codes[alphabet[idx]] = alphabet.charCodeAt(idx) - a
  }

  return grid.map(r => r.map(c => codes[c]))
}

const find = (grid, c) => {
  for(let row = 0; row < grid.length; row++) {
    for(let col = 0; col < grid[0].length; col++) {
      if(grid[row][col] === c) {
        return [row, col]
      }
    }
  }
  throw("Not found " + c)
}

const diff = (a, b) => Math.abs(a-b)

const walk = (grid, start, next, done) => {
  const height = grid.length
  const width = grid[0].width

  grid = convert(grid)

  const seen = {}

  const q = [[start, 0]]
  while(0 < q.length) {
    const [[x, y], d] = q.shift()

    if(seen[x+"-"+y]) {
      continue
    }
    seen[x+"-"+y] = true

    if(done(x, y)) {
      return d
    }

    const cur = grid[x][y]
    for(let [nx, ny] of [[x-1, y], [x+1, y], [x, y-1], [x, y+1]]) {
      if(seen[nx+"-"+ny]) { continue }
      if(nx < 0 || height <= nx) { continue }
      if(ny < 0 || width <= ny) { continue }

      if(next(cur, grid[nx][ny])) {
        q.push([[nx, ny], d+1])
      }
    }
  }
  throw("No route found")
}

console.log("Part1:", walk(grid,
                           find(grid, 'S'),
                           (cur, dst) => dst <= cur+1,
                           (x, y) => grid[x][y] == 'E'))

console.log("Part2:", walk(grid,
                           find(grid, 'E'),
                           (dst, cur) => dst <= cur+1,
                           (x, y) => grid[x][y] == 'a' || grid[x][y] == 'S'))
