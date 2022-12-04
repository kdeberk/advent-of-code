const shared = require('./shared.js')

const range = (a, b) => [...Array(b-a+1).keys()].map(x => a+x)

const fs = require('fs')

const input = fs.readFileSync('./input/day4.txt', 'utf8')

const lines = input.
      split("\n").
      slice(0, -1).
      map(l => l.split(",").
          map(x => x.split("-").
              map(i => i * 1)))

console.log("Part1:", lines.
            count(([[a1, a2], [b1, b2]]) => (a1 <= b1 && b2 <= a2) || b1 <= a1 && a2 <= b2))

console.log("Part2:", lines.
            count(([[a1, a2], [b1, b2]]) => range(a1, a2).
                  intersect(range(b1, b2)).
                  count()))
