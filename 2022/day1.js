
const fs = require('fs')

const input = fs.readFileSync('./input/day1.txt', 'utf8')

const calPerElf = input.split('\n\n')
      .map((elf) => elf.split('\n')
	   .map(line => (line * 1))
	   .reduce((acc, cur) => (acc + cur)))

console.log("Part 1: ", calPerElf.reduce((max, cur) => Math.max(max, cur)))

const byCalory = calPerElf.sort((a, b) => (a - b)).reverse()

console.log("Part 2: ", byCalory.slice(0, 3).reduce((acc, cur) => acc + cur))
