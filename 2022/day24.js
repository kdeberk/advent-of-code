"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day24.txt', 'utf8')

const mod = (m, n) => {
  while(m < 0) {
    m += n
  }
  return m % n
}

class Blizzards  {
  constructor(bs, w, h) {
    this.m = {}
    for(const [[x, y], c] of bs) {
      this.m[(x-1)+"-"+(y-1)+c] = true
    }
    this.w = w
    this.h = h
  }

  hit([x, y], t) {
    x -= 1
    y -= 1
    return this.m[mod(x - t, this.w) + "-" + y + ">"] ||
      this.m[mod(x + t, this.w) + "-" + y + "<"] ||
      this.m[x + "-" + mod(y - t, this.h) + "v"] ||
      this.m[x + "-" + mod(y + t, this.h) + "^"]
  }
}

const collectBlizzards = (input) => {
  const lines = input.lines()
  return new Blizzards(input.lines()
                            .map((l, y) => l.chars()
                                            .map((c, x) => {
                                              if(c != '.' && c != '#') {
                                                const d = {'>': [1, 0], '<': [-1, 0], '^': [0, -1], 'v': [0, 1]}[c]
                                                return [[x, y], c]
                                              }
                                              return null
                                            })
                                            .filter(x => x))
                            .reduce((acc, bs) => acc.concat(bs), []),
                       lines[0].length - 2, lines.length - 2)
}

class Heap {
  constructor(h) {
    this.items = []
    this.h = h
  }

  push(xy, t) {
    const item = {xy: xy, t: t, score: t + this.h(xy)}
    this.items.push(item)
    let idx = this.items.length - 1
    while(0 < idx) {
      const pIdx = Math.floor(idx/2)
      if(this.items[pIdx].score < item.score) {
        return
      }
      this.swap(idx, pIdx)
      idx = pIdx
    }
  }

  pop() {
    this.swap(0, this.items.length-1)
    const result = this.items.pop()

    let idx = 0
    while(true) {
      let cIdx = 2*idx+1
      if(this.items.length <= cIdx) {
        break
      }

      if(this.items[cIdx] < this.items[idx]) {
        this.swap(idx, cIdx)
        idx = cIdx
        continue
      }

      cIdx = 2*idx+2
      if(this.items.length <= cIdx) {
        break
      }

      if(this.items[cIdx] < this.items[idx]) {
        this.swap(idx, cIdx)
        idx = cIdx
        continue
      }

      break
    }

    return [result.xy, result.t]
  }

  swap(idx, jdx) {
    const tmp = this.items[idx]
    this.items[idx] = this.items[jdx]
    this.items[jdx] = tmp
  }
}

const moves = [[-1, 0], [1, 0], [0, 0], [0, -1], [0, 1]]
const dist = ([ax, ay], [bx, by]) => Math.abs(ax-bx)+Math.abs(ay-by)

const walk = (grid, blizzards, src, dst, t) => {
  const seen = {}

  const h = new Heap(xy => dist(xy, dst))
  h.push(src, t)

  let best = 1000

  while(0 < h.items.length) {
    const [[x, y], t] = h.pop()

    const k = x+"-"+y+":"+t
    if(seen[k] != undefined) {
      continue
    }
    seen[k] = true

    const g = dist([x, y], dst)
    if(best <= t + g) {
      continue
    }

    if(g == 0 && t < best) {
      best = t
      continue
    }

    for(const [dx, dy] of moves) {
      const [nx, ny] = [x+dx, y+dy]
      if(ny < 0 || grid.length <= ny || nx < 0 || grid[ny].length <= nx) {
        continue
      }

      const k = nx+"-"+ny+":"+(t+1)
      if(seen[k] != undefined) {
        continue
      }

      if(grid[ny][nx] == '#' || blizzards.hit([nx, ny], t+1)) {
        continue
      }
      h.push([nx, ny], t+1)
    }
  }

  return best
}

const blizzards = collectBlizzards(input)
const grid = input.grid()

const src = [grid[0].indexOf('.'), 0]
const dst = [grid[grid.length-1].indexOf('.'), grid.length-1]

console.log("Part1:", walk(grid, blizzards, src, dst, 0))
console.log("Part2:", [[src, dst], [dst, src], [src, dst]]
            .reduce((t, [src, dst]) => walk(grid, blizzards, src, dst, t), 0))
