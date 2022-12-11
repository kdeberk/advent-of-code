const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day5.txt', 'utf8')
const lines = input.lines()

const readStack = () => {
  const grid = lines.slice(0, 8)
  return [1, 5, 9, 13, 17, 21, 25, 29, 33]
    .map(idx => grid.map(l => l[idx])
                    .reverse()
                    .filter(c => c && c != ' '))
}

const moves = lines.slice(10).map(l => {
  xs = l.split(" ")
  return [xs[1]*1, xs[3]-1, xs[5]-1]
})

const doMoves = (arr, moves, fn) => {
  return moves.reduce(fn, arr)
              .map(s => s.last())
              .join("")
}


console.log("Part1:", doMoves(readStack(),
                              moves,
                              (arr, [n, from, to]) => {
                                n.times(_ => {
                                  arr[to].push(arr[from].pop())
                                })
                                return arr
                              }))

console.log("Part2:", doMoves(readStack(),
                              moves,
                              (arr, [n, from, to]) => {
                                [arr[from], y] = arr[from].split(arr[from].length - n)
                                arr[to] = arr[to].concat(y)
                                return arr
                              }))
