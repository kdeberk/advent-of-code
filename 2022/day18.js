"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day18.txt', 'utf8')

const AIR = ' '
const LAVA = 'x'
const WATER = '~'

const makeGrid = () => {
  const grid = (maxX+1).times(_ => {
    return (maxY+1).times(_ => new Array(maxZ+1).fill(AIR))
  })

  droplets.each(p => grid[p.x][p.y][p.z] = LAVA)
  return grid
}

class Point {
  constructor(x, y, z) {
    this.x = x
    this.y = y
    this.z = z
  }

  inside() {
    return 0 <= this.x && this.x <= maxX && 0 <= this.y && this.y <= maxY && 0 <= this.z && this.z <= maxZ
  }

  neighbors() {
    return [[0, 0, 1], [0, 0, -1], [0, 1, 0], [0, -1, 0], [1, 0, 0], [-1, 0, 0]]
      .map(([dx, dy, dz]) => new Point(this.x+dx, this.y+dy, this.z+dz))
  }
}

const countSurfaces = (grid, droplets, to) => {
  let count = 0
  for(const droplet of droplets) {
    count += droplet.neighbors().count(p => !p.inside() || grid[p.x][p.y][p.z] == to)
  }
  return count
}

const immerseInWater = (grid) => {
  const q = []
  const seen = {}

  for(let x = 0; x <= maxX; x++) {
    for(let y = 0; y <= maxY; y++) {
      for(let z = 0; z <= maxZ; z++) {
        if(grid[x][y][z] != AIR) {
          continue
        }

        const p = new Point(x, y, z)
        if(0 < p.neighbors().count(n => !n.inside())) {
          q.push(p)
        }
      }
    }
  }

  while(0 < q.length) {
    const cur = q.shift()
    const k = cur.x+"-"+cur.y+"-"+cur.z
    if(seen[k]) {
      continue
    }
    seen[k] = true
    grid[cur.x][cur.y][cur.z] = WATER
    cur.neighbors().filter(n => n.inside() && grid[n.x][n.y][n.z] == AIR)
       .each(n => q.push(n))
  }
}

const droplets = input.lines().map(l => new Point(...l.split(",").map(x => x*1)))

const maxX = droplets.map(p => p.x).max()
const maxY = droplets.map(p => p.y).max()
const maxZ = droplets.map(p => p.z).max()

const grid = makeGrid()
console.log("Part1:", countSurfaces(grid, droplets, AIR))
immerseInWater(grid)
console.log("Part2:", countSurfaces(grid, droplets, WATER))
