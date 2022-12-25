require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day3.txt', 'utf8')

const lower = "abcdefghijklmnopqrstuvwxyz"
const upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

const lowerPriority = (x) => lower.includes(x) ? lower.indexOf(x) + 1 : 0
const upperPriority = (x) => upper.includes(x) ? upper.indexOf(x) + 27 : 0
const priority = (x) => lowerPriority(x) + upperPriority(x)

console.log("Part1:", input.split("\n")
                           .slice(0, -1)
                           .map(line => {
                             [a, b] = [line.slice(0, line.length/2), line.slice(line.length/2)]
                             return a.chars()
                                     .intersect(b.chars())
                                     .map(priority)
                                     .sum()
                           })
                           .sum())

console.log("Part2:", input.split("\n")
                           .slice(0, -1)
                           .chunks(3)
                           .map(ch => {
                             [a, b, c] = ch.map(x => x.chars())
                             return a.intersect(b, c)
                           })
                           .map(priority)
                           .sum())
