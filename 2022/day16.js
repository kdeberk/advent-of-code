"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day16_test.txt', 'utf8')

// TODO: this is broken. Doesn't work!

class Valve {
  constructor(name, flow, neighbors) {
    this.idx = 0
    this.name = name
    this.flow = flow
    this.neighbors = neighbors
    this.distances = {}
  }
}

const parseValves = (input) => {
  const parseLine = (line) => {
    const frags = line.split(" ")
    const name = frags[1]
    const flow = [...frags[4].matchAll(/rate=(\d+);/)][0][1] * 1
    const neighbors = frags.slice(9).join('').split(',')
                           .reduce((acc, n) => {acc[n] = 1; return acc}, {})

    return new Valve(name, flow, neighbors)
  }

  const removeBrokenValves = (valves) => {
    while(true) {
      const idx = valves.findIndex(v => v.flow == 0 && v.name != 'AA')
      if(idx == -1) {
        break
      }
      const [ cur ] = valves.splice(idx, 1)
      const ns = Object.keys(cur.neighbors)
      for(let idx = 0; idx < ns.length; idx++) {
        for(let jdx = idx+1; jdx < ns.length; jdx++) {
          const from = byName[ns[idx]]
          const to = byName[ns[jdx]]
          from.neighbors[to.name] = cur.neighbors[from.name] + cur.neighbors[to.name]
          to.neighbors[from.name] = cur.neighbors[from.name] + cur.neighbors[to.name]
          delete from.neighbors[cur.name]
          delete to.neighbors[cur.name]
        }
      }
    }
    return valves
  }

  const calcDistance = (valve, cur, d) => {
    const best = valve.distances[cur.name]
    if(best < d) {
      return
    }
    valve.distances[cur.name] = d
    for(const n of Object.keys(cur.neighbors)) {
      calcDistance(valve, byName[n], d+cur.neighbors[n])
    }
  }

  let valves = input.lines().map(parseLine)
  const byName = valves.reduce((acc, v) => {acc[v.name] = v; return acc}, {})

  valves = removeBrokenValves(valves)
  valves.each(valve => calcDistance(valve, valve, 0))
  valves.each((valve, idx) => { valve.idx = 1<<idx })
  return valves.reduce((acc, v) => {acc[v.name] = v; return acc}, {})
}

const max = Math.max
const solve = (valves, seen, cur, closed, flow, score, minLeft, walkersLeft, start, min) => {
  if(minLeft < 0) {
    return 0
  } else if(minLeft == 0) {
    return score
  }

  // 15 bits are reserved for the cur.idx values.
  // 30 minutes is at most 5 bits, so 5 digits are reserved.
  const k = (((flow*64 | closed)*32 | cur.idx)*32 | minLeft)//*4// | walkersLeft
  if(seen[k] != undefined) {
    return seen[k]
  }

  let best = score
  for(const n of Object.keys(cur.distances)) {
    if(n == 'AA') {
      continue
    }
    if(0 < (valves[n].idx & closed)) {
      continue
    }
    const dst = valves[n]

    best = max(best, solve(valves,
                           seen,
                           dst,
                           closed | dst.idx,
                           flow + dst.flow,
                           score + dst.flow * (minLeft-cur.distances[n]-1),
                           minLeft - cur.distances[n] - 1,
                           walkersLeft,
                           start,
                           min))
  }

  if(best == 1651) {
    console.log(closed, cur.idx, minLeft)
  }

  seen[k] = best
  return best
}

const valves = parseValves(input)
console.log("Part1:", solve(valves, {}, valves['AA'], 0, 0, 0, 30, 0))
// console.log("Part2:", solve(valves, {}, valves['AA'], 0, 0, 26, 1, valves['AA'], 26))




// Day 2 has 15 valves to consider.
// Two walkers.

// class Heap {
//   constructor() {
//     this.items = []
//   }

//   push(item) {
//     this.items.push(item)

//     let idx = this.items.length - 1
//     while(0 < idx) {
//       const pIdx = Math.floor(idx / 2)
//       if(item.score <= this.items[pIdx].score) {
//         break
//       }
//       this.swap(idx, pIdx)
//       idx = pIdx
//     }
//   }

//   pop() {
//     this.swap(0, this.items.length-1)
//     const result = this.items.pop()

//     let idx = 0
//     const item = this.items[idx]
//     while (true) {
//       let cIdx = 2*idx + 1
//       if(this.items.length <= cIdx) {
//         break
//       }

//       if(item.score < this.items[cIdx].score) {
//         this.swap(idx, cIdx)
//         idx = cIdx
//         continue
//       }

//       cIdx++
//       if(this.items.length <= cIdx) {
//         break
//       }

//       if(item.score < this.items[cIdx].score) {
//         this.swap(idx, cIdx)
//         idx = cIdx
//         continue
//       }

//       break
//     }

//     return result
//   }

//   swap(idx, jdx) {
//     const tmp = this.items[idx]
//     this.items[idx] = this.items[jdx]
//     this.items[jdx] = tmp
//   }
// }


// Use dynamic programming?
// Can't use per minute, since a lower score at minute x can lead to the best score on minute x+n
// Three dimensional array?
// - Not really, since we also have to keep track of open/closed state


// class State {
//   constructor(valves) {
//     this.score = 0
//     this.minute = 0
//     this.transit = 0
//     this.open_valves = {'AA': true}
//     this.cur = valves['AA']
//     this.valves = valves
//   }

//   nexts() {
//     this.minute++

//     if(0 < this.transit) {
//       this.transit--
//       return [this]
//     }

//     if(!this.open_valves[this.cur.name]) {
//       this.open_valves[this.cur.name] = true
//       this.score += (30 - this.minute) * this.cur.flow
//       return [this]
//     }

//     const nxts = []
//     for(const n of Object.keys(this.cur.distances)) {
//       if(this.open_valves[n]) {
//         continue
//       }
//       const nxt = this.clone()
//       nxt.transit = this.cur.distances[n] - 1
//       nxt.cur = this.valves[n]
//       nxts.push(nxt)
//     }
//     return nxts
//   }

//   done() {
//     return this.open_valves.length() == this.valves.length()
//   }

//   left() {
//     return Object.keys(this.cur.distances)
//                  .filter(n => !this.open_valves[n])
//                  .map(n => valves[n].flow * (30 - (this.minute + this.cur.distances[n])))
//                  .filter(n => 0 < n)
//                  .sum()
//   }

//   clone() {
//     const c = Object.create(State.prototype)
//     c.score = this.score
//     c.minute = this.minute
//     c.transit = this.transit
//     c.open_valves = this.open_valves.map(_ => true)
//     c.cur = this.cur
//     c.valves = this.valves
//     return c
//   }
// }

// class DoubleState {
//   constructor(valves) {
//     this.score = 0
//     this.minute = 0
//     this.cur_transit = 0
//     this.ele_transit = 0
//     this.open_valves = {'AA': true}
//     this.cur = valves['AA']
//     this.ele = valves['AA']
//     this.valves = valves
//   }

//   nexts() {
//     this.minute++

//     const humans = []
//     if(0 < this.cur_transit) {
//       this.cur_transit--
//       humans.push(this)
//     } else if(!this.open_valves[this.cur.name]) {
//       this.open_valves[this.cur.name] = true
//       this.score += (26 - this.minute) * this.cur.flow
//       humans.push(this)
//     } else {
//       for(const n of Object.keys(this.cur.distances)) {
//         if(this.open_valves[n]) {
//           continue
//         }
//         const nxt = this.clone()
//         nxt.cur_transit = this.cur.distances[n] - 1
//         nxt.cur = this.valves[n]
//         humans.push(nxt)
//       }
//     }

//     const both = []
//     for(const human of humans) {
//       if(0 < human.ele_transit) {
//         human.ele_transit--
//         both.push(human)
//         continue
//       }

//       if(!human.open_valves[human.ele.name]) {
//         human.open_valves[human.ele.name] = true
//         human.score += (26 - human.minute) * human.ele.flow
//         both.push(human)
//         continue
//       }

//       for(const n of Object.keys(this.ele.distances)) {
//         if(human.open_valves[n] && n != human.cur.name) {
//           continue
//         }
//         const nxt = human.clone()
//         nxt.ele_transit = human.ele.distances[n] - 1
//         nxt.ele = human.valves[n]
//         both.push(nxt)
//       }
//     }

//     return both
//   }

//   done() {
//     return this.open_valves.length() == this.valves.length()
//   }

//   left() {
//     const human = Object.keys(this.cur.distances)
//                  .filter(n => !this.open_valves[n])
//                  .map(n => valves[n].flow * (26 - (this.minute + this.cur.distances[n])))
//                  .sum()
//     const elephant = Object.keys(this.ele.distances)
//                  .filter(n => !this.open_valves[n])
//                  .map(n => valves[n].flow * (26 - (this.minute + this.ele.distances[n])))
//                  .sum()
//     return human+elephant
//   }

//   clone() {
//     const c = Object.create(DoubleState.prototype)
//     c.score = this.score
//     c.minute = this.minute
//     c.cur_transit = this.cur_transit
//     c.ele_transit = this.ele_transit
//     c.open_valves = this.open_valves.map(_ => true)
//     c.cur = this.cur
//     c.ele = this.ele
//     c.valves = this.valves
//     return c
//   }
// }

// const closeValves = (start, minutesLeft) => {
//   const h = new Heap()
//   h.push(start)

//   const seen = {}

//   let best = 0
//   while(0 < h.items.length) {
//     const cur = h.pop()
//     if(best < cur.score) {
//       best = cur.score
//       console.log(cur.score)
//     }
//     if(cur.done()) {
//       continue
//     } else if(cur.minute == minutesLeft) {
//       continue
//     }

//     const k = cur.minute + "-" + Object.keys(cur.open_valves).sort().join('')
//     if(cur.score < seen[k]) {
//       continue
//     }
//     seen[k] = cur.score

//     if(cur.score + cur.left() <= best) {
//       continue
//     }

//     cur.nexts().each(nxt => h.push(nxt))
//   }

//   return best
// }


// console.log("Part2:", closeValves(new DoubleState(valves)))

// not 4870, 4894, 2258, 2262
