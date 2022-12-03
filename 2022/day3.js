const fs = require('fs')

const input = fs.readFileSync('./input/day3.txt', 'utf8')

const lower = "abcdefghijklmnopqrstuvwxyz"
const upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

const lowerPriority = (x) => lower.includes(x) ? lower.indexOf(x) + 1 : 0
const upperPriority = (x) => upper.includes(x) ? upper.indexOf(x) + 27 : 0
const priority = (x) => lowerPriority(x) + upperPriority(x)

const unique = (array) => {
  const h = array.
        reduce((h, cur) => {
          h[cur] = h[cur] ? h[cur]+1 : 1
          return h
        }, {})
  return Object.keys(h)
}

const intersection = (a, b) => a.filter(x => b.includes(x))
const sum = (a, b) => a + b
const chunks = (array, l) => Array(array.length / l).
  fill(0).
  map((_, i) => array.slice(i * 3, (i+1) * 3))

console.log(
  "Part1:",
  input.
    split("\n").slice(0, -1).
    map(line => {
      [a, b] = [line.slice(0, line.length/2), line.slice(line.length/2)]
      return intersection(unique(a.split("")), unique(b.split(""))).
        map(priority).
        reduce(sum)
    }).reduce(sum))



console.log(
  "Part2:",
  chunks(input.split("\n").slice(0, -1), 3).
    map(ch => {
      [a, b, c] = ch.map(x => unique(x.split("")))
      return intersection(intersection(a, b), c)
    }).
    map(priority).
    reduce(sum)
)
