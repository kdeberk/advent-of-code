const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day13.txt', 'utf8')

const inOrder = -1
const checkMore = 0
const outOfOrder = 1

const compare = (a, b) => {
  const numbera = typeof(a) === 'number'
  const numberb = typeof(b) === 'number'

  if(numbera && numberb) {
    return a < b ? inOrder : a == b ? checkMore : outOfOrder
  } else if(numbera) {
    return compare([a], b)
  } else if(numberb) {
    return compare(a, [b])
  } else {
    let idx = 0;
    for(; idx < a.length; idx++) {
      if(idx === b.length) {
        return outOfOrder
      }

      switch (compare(a[idx], b[idx])) {
        case inOrder:
          return inOrder
        case checkMore:
          continue
        case outOfOrder:
          return outOfOrder
      }
    }

    return (a.length === b.length) ? checkMore : inOrder
  }
}

console.log("Part1:", input.split("\n\n")
                           .map(p => p.split("\n")
                                     .slice(0, 2)
                                     .map(eval))
                           .map(([a, b], idx) => compare(a, b) === inOrder ? idx+1 : 0)
                           .sum())

const dividers = ['[[2]]', '[[6]]']
const sorted = input.lines()
                    .filter(l => 0 < l.length)
                    .concat(dividers)
                    .map(eval)
                    .sort((a, b) => compare(a, b))
                    .map(JSON.stringify)

console.log("Part2:", dividers.map(d => sorted.indexOf(d))
                              .map(idx => idx+1)
                              .product())
