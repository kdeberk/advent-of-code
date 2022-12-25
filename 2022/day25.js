"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day25.txt', 'utf8')

const fromSnafu = (s) => {
  const t = {'2': 2, '1': 1, '0': 0, '-': -1, '=': -2}
  return s.chars()
          .map((c, idx) => t[c] * Math.floor(Math.pow(5, s.length-idx-1)))
          .sum()
}

const toSnafu = (n) => {
  const s = []
  for(let p = Math.pow(5, Math.ceil((Math.log(n)/Math.log(5)))); 0 < p; p = Math.floor(p / 5)) {
    const m = Math.round(n / p)

    switch(m) {
      case -2: s.push('='); break
      case -1: s.push('-'); break
      case  0: s.push('0'); break
      case  1: s.push('1'); break
      case  2: s.push('2'); break
    }
    n -= p*m
  }

  while(0 < s.length && s[0] == '0') {
    s.shift()
  }

  return s.join('')
}

console.log("Part1:", toSnafu(input.lines().map(fromSnafu).sum()))
