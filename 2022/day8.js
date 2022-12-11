const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day8.txt', 'utf8')

const grid = input.lines()
                  .map(l => l.chars().map(c => c*1))

const [height, width] = [grid.length, grid[0].length]

const visible = grid.map(_ => Array(width).fill(false))

for(let col = 0; col < width; col++) {
  visible[0][col] = true
  visible[height-1][col] = true
}

for(let row = 0; row < height; row++) {
  visible[row][0] = true
  visible[row][width-1] = true
}

// View from left
for(let row = 1; row < height-1; row++) {
  let max = grid[row][0]
  for(let col = 1; col < width-1; col++) {
    const cur = grid[row][col]
    if(max < cur) {
      visible[row][col] = true
      max = cur
    }
  }
}

// View from right
for(let row = 1; row < height-1; row++) {
  let max = grid[row][width-1]
  for(let col = width-2; 0 < col; col--) {
    const cur = grid[row][col]
    if(max < cur) {
      visible[row][col] = true
      max = cur
    }
  }
}

// View from above
for(let col = 1; col < width-1; col++) {
  let max = grid[0][col]
  for(let row = 1; row < height-1; row++) {
    const cur = grid[row][col]
    if(max < cur) {
      visible[row][col] = true
      max = cur
    }
  }
}

for(let col = 1; col < width-1; col++) {
  let max = grid[height-1][col]
  for(let row = height-2; 0 < row; row--) {
    const cur = grid[row][col]
    if(max < cur) {
      visible[row][col] = true
      max = cur
    }
  }
}

let count = 0
for(let row = 0; row < height; row++) {
  for(let col = 0; col < width; col++) {
    if(visible[row][col]) {
      count++
    }
  }
}

console.log("Part1:", count)

const scores = grid.map(_ => Array(width).fill(1))
const diff = (a, b) => Math.abs(a-b)

for(let row = 1; row < height-1; row++) {
  for(let col = 1; col < width-1; col++) {
    let cur = grid[row][col]

    // Up
    let up = row-1
    for(; 0 <= up; up--) {
      let tree = grid[up][col]
      if(cur <= tree) {
        scores[row][col] *= diff(row, up)
        break
      }
    }
    if(up == -1) {
      scores[row][col] *= diff(row, 0)
    }

    // Down
    let down = row+1
    for(; down < height; down++) {
      let tree = grid[down][col]
      if(cur <= tree) {
        scores[row][col] *= diff(row, down)
        break
      }
    }
    if(down == height) {
      scores[row][col] *= diff(row, height-1)
    }

    // Left
    let left = col-1
    for(; 0 <= left; left--) {
      let tree = grid[row][left]
      if(cur <= tree) {
        scores[row][col] *= diff(col, left)
        break
      }
    }
    if(left == -1) {
      scores[row][col] *= diff(col, 0)
    }

    // Right
    let right = col+1
    for(; right < width; right++) {
      let tree = grid[row][right]
      if(cur <= tree) {
        scores[row][col] *= diff(col, right)
        break
      }
    }
    if(right == width) {
      scores[row][col] *= diff(col, width-1)
    }

  }
}


let best = 0
for(let row = 0; row < height; row++) {
  for(let col = 0; col < width; col++) {
    if(best < scores[row][col]) {
      best = scores[row][col]
    }
  }
}

console.log("Part2:", best)
