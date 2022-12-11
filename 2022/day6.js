require('./shared.js')

const fs = require('fs')
const line = fs.readFileSync('./input/day6.txt', 'utf8')

const findMarker = (line, len) => {
  for(i = 0; i < line.length; i++) {
    if (line.slice(i, i+len)
            .chars()
            .unique()
            .length == len) {
      return i+len
    }
  }
}

console.log("Part1:", findMarker(line, 4))
console.log("Part2:", findMarker(line, 14))
