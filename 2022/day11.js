const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day11.txt', 'utf8')

const primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

class Number {
  constructor(n) {
    this.n = n
    this.mods = {}
    primes.each(p => {
      this.mods[p] = n % p
    })
  }

  add(n) {
    this.n += n
    primes.each(p => {
      this.mods[p] = (this.mods[p] + n) % p
    })
  }

  div(n) {
    if(n === 1) {
      return
    }

    this.n = Math.floor(this.n / n)
    this.mods = {}
    primes.each(p => {
      this.mods[p] = this.n % p
    })
  }

  mul(n) {
    this.n *= n
    primes.each(p => {
      this.mods[p] = (this.mods[p] * n) % p
    })
  }

  square() {
    this.n *= this.n
    primes.each(p => {
      this.mods[p] = (this.mods[p] * this.mods[p]) % p
    })
  }

  divisibleByPrime(p) {
    return this.mods[p] === 0
  }
}

class Monkey {
  constructor(items, op, mod, ifTrue, ifFalse) {
    this.nInspects = 0
    this.items = items
    this.op = op
    this.mod = mod
    this.ifTrue = ifTrue
    this.ifFalse = ifFalse
  }

  monkeyAround(monkeys, div) {
    while(0 < this.items.length) {
      this.nInspects++

      let worry = this.items.shift()
      this.op(worry)
      worry.div(div)

      const dst = worry.divisibleByPrime(this.mod) ? this.ifTrue : this.ifFalse
      monkeys[dst].items.push(worry)
    }
  }
}

const parseMonkey = (text) => {
  const lines = text.lines()

  const parseExpr = (expr) => {
    const [_, op, val] = expr.split(" ")
    switch(op) {
      case "+":
        return (x) => x.add(val * 1)
      case "*":
        if(val === "old") {
          return (x) => x.square()
        }
        return (x) => x.mul(val * 1)
      default:
        throw("Could not parse "+expr)
    }
  }

  return new Monkey(
    lines[1].split(":")[1].split(", ").map(x => new Number(x*1)),
    parseExpr(lines[2].split(":")[1].split(" = ")[1]),
    lines[3].split(" ").last(),
    lines[4].split(" ").last() * 1,
    lines[5].split(" ").last() * 1,
  )
}



const keepAway = (nRounds, div) => {
  const monkeys = input.split("\n\n")
                       .map(parseMonkey)


  for(let round = 0; round < nRounds; round++) {
    monkeys.each(m => m.monkeyAround(monkeys, div))
  }
  return monkeys.map(m => m.nInspects)
                .sort((a, b) => (b-a))
                .slice(0, 2)
                .product()
}

console.log("Part1:", keepAway(20, 3))
console.log("Part2:", keepAway(10000, 1))
