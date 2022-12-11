const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day7.txt', 'utf8')

class Dir {
  constructor(name, parent) {
    this.name = name
    this.children = {}
    this.parent = parent
    this.size = 0
  }

  addChild(node) {
    this.children[node.name] = node
  }

  walk(fn) {
    Object.values(this.children)
          .each(ch => ch.walk(fn))
    fn(this)
  }

  dirs() {
    return Object.values(this.children)
                 .map(dir => [dir, dir.dirs()])
                 .reduce((a, b) => a.concat(...b), [])
  }
}

const root = new Dir("/", null)

let cur = root
for (const x of input.split("$ ").slice(2)) {
  const lines = x.lines()
  const [cmd, arg] = lines[0].split(" ")

  switch(cmd) {
    case "cd":
      if(arg === "..") {
        cur = cur.parent
        break
      }
      cur = cur.children[arg]
      break
    case "ls":
      lines.slice(1)
           .each(f => {
             const  [t, name] = f.split(" ")
             if(t === "dir") {
               cur.addChild(new Dir(name, cur))
               return
             }
             cur.size += t * 1
           })
  }
}

root.walk(dir => {
  if(dir.parent !== null) {
    dir.parent.size += dir.size
  }
})

console.log("Part1:",
            root.dirs()
                .filter(dir => dir.size < 100_000)
                .map(dir => dir.size)
                .sum())

const diskSize = 70_000_000
const neededSize = 30_000_000
const toDelete = root.size - (diskSize - neededSize)

console.log("Part2:",
            root.dirs()
                .map(dir => dir.size)
                .filter(sz => toDelete < sz)
                .min())
