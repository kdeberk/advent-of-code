
require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day1.txt', 'utf8')

const calPerElf = input.split('\n\n').
      map((elf) => elf.split('\n').
          map(line => (line * 1)).
          sum())

console.log("Part1:", calPerElf.max())

console.log("Part2:",
            calPerElf.sort((a, b) => (b - a)).
            slice(0, 3).
            sum())
