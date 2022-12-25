const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day15.txt', 'utf8')

const sign = a => 0 < a ? -1 : a == 0 ? 0 : 1

// Line expresses y=cx+n where c is either -1 or 1 and n can be negative.
class Line {
  constructor(c, n) {
    this.c = c
    this.n = n
  }

  overlap(other) {
    if(this.c == other.c && this.n == other.n) {
      return null // Same line
    } else if(this.c == other.c) {
      return null // Parallel
    } else {
      const x = (other.n - this.n) / (this.c - other.c)
      const y = this.c * x + this.n
      return new Coord(x, y)
    }
  }
}

// Line is y=x+n or y=-x+n

class Coord {
  constructor(x, y) {
    this.x = x
    this.y = y
  }

  dist(other) {
    return Math.abs(this.x-other.x) + Math.abs(this.y-other.y)
  }
}

class Beacon extends Coord {}

class Sensor extends Coord {
  constructor(x, y, range) {
    super(x, y)
    this.range = range
  }

  covers(other) {
    return this.dist(other) <= this.range
  }

  boundaries() {
    return [
      new Line(1, this.y-this.x-this.range-1),
      new Line(1, this.y-this.x+this.range+1),
      new Line(-1, this.x+this.y-this.range-1),
      new Line(-1, this.x+this.y+this.range+1),
    ]
  }
}

const allNumbers = /-?\d+/g

const coords = input.lines()
                    .map(l => [...l.matchAll(allNumbers)].map(m => m[0] * 1))
                    .map(([sx, sy, bx, by]) => {
                      const beacon = new Beacon(bx, by)
                      const sensor = new Sensor(sx, sy, (new Coord(sx, sy).dist(beacon)))
                      return [sensor, beacon]
                    })
                    .reduce((a, b) => a.concat(b))

const sensors = coords.filter(c => c instanceof Sensor)
const beacons = coords.filter(c => c instanceof Beacon)

const SENSOR = 'S'
const BEACON = 'B'
const COVERED = '#'
const UNCOVERED = '.'

const countCovered = (sensors, beacons, y) => {
  const minX = sensors.map(s => s.x - s.range).min()
  const maxX = sensors.map(s => s.x + s.range).max()
  const line = (new Array(maxX - minX + 1).fill(UNCOVERED))

  sensors.filter(s => s.y == y).each(s => line[s.x - minX] = SENSOR)
  beacons.filter(b => b.y == y).each(b => line[b.x - minX] = BEACON)

  for(let s of sensors) {
    const vy = s.range - Math.abs(s.y-y)
    if(vy < 0) {
      continue
    }

    for(let x = s.x - vy - 1; x < s.x + vy; x++) {
      if(line[x - minX] == UNCOVERED) {
        line[x - minX] = COVERED
      }
    }
  }
  return line.count(c => c == COVERED)
}

const findUncovered = (sensors, maxX, maxY) => {
  const lines = sensors.map(s => s.boundaries()).reduce((a, b) => a.concat(b))
  const counts = lines.reduce((m, l) => {
    m[l.c+"-"+l.n] = (m[l.c+"-"+l.n] || 0) + 1;
    return m
  }, {})
  const dups = lines.filter(l => 1 < counts[l.c+"-"+l.n])

  for(let i = 0; i < dups.length; i++) {
    SearchLoop:
    for(let j = i+1; j < dups.length; j++) {
      const c = dups[i].overlap(dups[j])
      if(c == null) {
        continue
      } else if(!(0 <= c.x && c.x < maxX && 0 <= c.y && c.y < maxY)) {
        continue
      }

      for(let s of sensors) {
        if(s.covers(c)) {
          continue SearchLoop
        }
      }
      return 4_000_000*c.x + c.y
    }
  }
}

console.log("Part1:", countCovered(sensors, beacons, 2_000_000))
console.log("Part2:", findUncovered(sensors, 40_000_000, 40_000_000))
