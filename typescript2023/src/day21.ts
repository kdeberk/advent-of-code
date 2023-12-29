import { uniq } from "./shared";

type Grid = string[][]

function parseInput(input: string): Grid {
    return input.split("\n").map((line) => line.split(""))
}

type Coord = {x: number, y: number}

function mod(n: number, m: number): number {
  return ((n % m) + m) % m;
}

function findStart(grid: Grid): Coord {
    let start = { x: -1, y: -1 };
    for(let y = 0; start.y === -1 && y < grid.length; y++) {
        for(let x = 0; start.x === -1 && x < grid[0].length; x++) {
            if(grid[y][x] === "S") {
                start.x = x
                start.y = y
            }
        }
    }
    return start
}

function countTiles(grid: Grid, start: Coord, max: number): Record<string, number> {
    const q: { cur: Coord, dist: number }[] = [{cur: start, dist: 0}]
    const seen: Record<string, number> = {}

    while(0 < q.length) {
        const { cur, dist } = q.shift()!
        if(max < dist) {
            continue
        }

        const k = `${cur.x}-${cur.y}`
        if(k in seen) {
            continue
        }

        seen[`${cur.x}-${cur.y}`] = dist

        for(const [dx, dy] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
            const nxt = { x: cur.x + dx, y: cur.y + dy }
            if(grid[mod(nxt.y, grid.length)][mod(nxt.x, grid[0].length)] === "#") {
                continue
            }

            q.push({ cur: nxt, dist: dist + 1})
        }
    }
    return seen
}

// TODO: investigate why this produces different results than countTiles
function countTiles2(grid: Grid, start: Coord, max: number): number {
    let cur:Coord[] = [start]
    for(let idx = 0; idx < max; idx++) {
        const nxt: Record<string, Coord> = {}

        for(const c of cur) {
            for (const [dx, dy] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
                const n = { x: c.x + dx, y: c.y + dy }
                if (grid[mod(n.y, grid.length)][mod(n.x, grid[0].length)] === "#") {
                    continue
                }
                const k = `${n.x}-${n.y}`
                nxt[k] = n
            }
        }
        cur = Object.values(nxt)
    }
    return cur.length
}


function part1(grid: Grid): number {
    const start = findStart(grid)
    const seen  = countTiles(grid, start, 64)
    return Object.values(seen).filter((n) => n % 2 === 0).length
}

function lagrange(points: Coord[], x: number): number {
    let result = 0

    for(let idx = 0; idx < points.length; idx++) {
        let term = points[idx].y
        for(let jdx = 0; jdx < points.length; jdx++) {
            if(idx === jdx) {
                continue
            }
            term *= (x - points[jdx].x) / (points[idx].x - points[jdx].x)
        }
        result += term
    }
    return result
}

function part2(grid: Grid): number {
    const start = findStart(grid)

    const points:Coord[] = []
    for(let x = 65, idx = 0; idx < 3; idx++, x += 131) {
        points.push({ x, y: countTiles2(grid, start, x) })
    }

    return lagrange(points, 26501365)
}

export const Day21 = {
    number: 21,
    title: "Step Counter",
    parseInput,
    part1,
    part2,
}
