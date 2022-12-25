"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day20.txt', 'utf8')

class Node {
  constructor(n) {
    this.prev = null
    this.next = null
    this.n = n
  }
}

const mod = (m, n) => {
  if(0 < m) {
    return m%n
  } else if(m == 0) {
    return 0
  } else {
    while(m < 0) {
      m+=n
    }
    return m%n
  }
}

const numbers = input.lines().map(l => l*1)

const buildList = (numbers) => {
  const nodes = numbers.map(n => new Node(n))
  for(let idx = 0; idx < nodes.length; idx++) {
    const cur = nodes[idx]
    cur.prev = nodes[mod(idx-1, nodes.length)]
    cur.next = nodes[mod(idx+1, nodes.length)]
  }
  return nodes
}

const abs = Math.abs

const mix = (order) => {
  for(const node of order) {
    if(node.n == 0) {
      continue // 0 doesn't move
    }

    let dst
    if(node.n < 0) {
      dst = node.prev

      node.next.prev = node.prev
      node.prev.next = node.next

      for(let i = -1; -1 * mod(abs(node.n), numbers.length-1) <= i; i--) {
        dst = dst.prev
      }
    } else {
      dst = node.next

      node.next.prev = node.prev
      node.prev.next = node.next

      for(let i = 1; i < mod(abs(node.n), numbers.length-1); i++) {
        dst = dst.next
      }
    }

    node.next = dst.next
    node.prev = dst

    node.prev.next = node
    node.next.prev = node
  }
}

const decrypt = (numbers, key, nMixes) => {
  numbers = numbers.map(x => x * key)
  let nodes = buildList(numbers)
  for(let n = 0; n < nMixes; n++) {
    mix(nodes)
  }

  const zero = nodes.find(n => n.n == 0)

  const xs = [0]
  for(let cur = zero.next; cur.n != 0; cur = cur.next) {
    xs.push(cur.n)
  }
  return [1000, 2000, 3000].map(idx => xs[idx % xs.length]).sum()
}

console.log("Part1:", decrypt(numbers, 1, 1))
console.log("Part2:", decrypt(numbers, 811589153, 10))
