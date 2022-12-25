"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day21.txt', 'utf8')

class Equation {
  constructor(a, b) {
    this.a = a
    this.b = b
  }
  add(o) {
    return new Equation(this.a, this.b+o.n)
  }
  sub(o) {
    return new Equation(this.a, this.b-o.n)
  }
  mul(o) {
    return new Equation(this.a*o.n, this.b*o.n)
  }
  div(o) {
    return new Equation(this.a/o.n, this.b/o.n)
  }
}

class KnownNumber {
  constructor(n) {
    this.n = n
  }
  add(o) {
    if(o instanceof Equation) {
      return o.add(this)
    }
    return new KnownNumber(this.n + o.n)
  }
  sub(o) {
    if(o instanceof Equation) {
      return new Equation(-o.a, this.n-o.b)
    }
    return new KnownNumber(this.n - o.n)
  }
  mul(o) {
    if(o instanceof Equation) {
      return o.mul(this)
    }
    return new KnownNumber(this.n * o.n)
  }
  div(o) {
    return new KnownNumber(this.n / o.n)
  }
  int() {
    return Math.round(this.n)
  }
}

const buildTable = (exprs) => {
  return exprs.map((name, expr) => {
    if(Number.isInteger(expr * 1)) {
      return new KnownNumber(expr * 1)
    }

    let [a, op, b] = expr.split(" ")
    switch(op) {
      case "+":
        op = (a, b) => a.add(b)
        break
      case "-":
        op = (a, b) => a.sub(b)
        break
      case "*":
        op = (a, b) => a.mul(b)
        break
      case "/":
        op = (a, b) => a.div(b)
        break
      case "=":
        op = (a, b) => new KnownNumber((b.n-a.b)/a.a)
    }

    return (exprs) => {
      if(!(exprs[a] instanceof Function) && !(exprs[b] instanceof Function)) {
        exprs[name] = op(exprs[a], exprs[b])
      }
    }
  })
}

const solveRoot = (table) => {
  while(true) {
    let found = false
    for(const name of Object.keys(table)) {
      if(table[name] instanceof Function) {
        found = true
        table[name](table)
      }
    }
    if(!found) {
      break
    }
  }
  return table['root'].int()
}

const prepare = (exprs) => {
  const [a, _, b] = exprs['root'].split(' ')
  exprs['root'] = a + " = " + b
  return exprs
}

const replaceHuman = (table) => {
  table['humn'] = new Equation(1, 0)
  return table
}

const exprs = input.lines()
                   .map(l => l.split(": "))
                   .reduce((acc, [name, expr]) => { acc[name] = expr; return acc }, {})

console.log("Part1:", solveRoot(buildTable(exprs)))
console.log("Part2:", solveRoot(replaceHuman(buildTable(prepare(exprs)))))



// table['humn'] = new Equation(1, 0)
