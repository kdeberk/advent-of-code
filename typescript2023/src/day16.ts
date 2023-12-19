import { uniq } from "./shared"

type Grid = string[][]

function parseInput(input: string): Grid {
    return input.split("\n").map((line) => line.split(""))
}

type Coord = { x: number, y: number }
type Direction = "N" | "S" | "E" | "W"
type Mirror = "/" | "\\" | "|" | "-"
type Light = { coord: Coord, dir: Direction }

function coordKey(c: Coord): string {
    return `${c.x}-${c.y}`
}

function move({ coord, dir }: Light): Light {
    const [dx, dy] = { "N": [0, -1], "S": [0, 1], "W": [-1, 0], "E": [1, 0] }[dir]
    return { coord: { x: coord.x + dx, y: coord.y + dy }, dir }
}

function hitMirror(light: Light, mirror: Mirror): Light[] {
    const dirs = {
        "N": { "/": ["E"], "\\": ["W"], "-": ["W", "E"], "|": ["N"]},
        "S": { "/": ["W"], "\\": ["E"], "-": ["W", "E"], "|": ["S"]},
        "E": { "/": ["N"], "\\": ["S"], "-": ["E"], "|": ["N", "S"]},
        "W": { "/": ["S"], "\\": ["N"], "-": ["W"], "|": ["N", "S"]},
    }[light.dir][mirror] as Direction[]

    return dirs.map((dir) => { return { dir, coord: light.coord }}).map(move)
}

function shineLight(grid: Grid, light: Light): number {
    const q: Light[] = [light]
    const seen = new Set()
    const seenDir = new Set()

    while(0 < q.length) {
        const light = q.shift()!
        const { dir, coord } = light;
        const { x, y } = coord;

        if(!(0 <= x && x < grid[0].length && 0 <= y && y < grid.length)) {
            continue
        }
        const k = coordKey(coord)
        if(seenDir.has(dir + k)) {
            continue
        }
        seenDir.add(dir + k)
        seen.add(k)

        if (grid[y][x] === ".") {
            q.push(move(light))
        } else {
            q.push(...hitMirror(light, grid[y][x] as Mirror))
        }
    }

    return Array.from(seen).length
}

function part1(grid: Grid): number {
    return shineLight(grid, { coord: { x: 0, y: 0 }, dir: "E"})
}

function part2(grid: Grid): number {
    let max = 0;
    for(let x = 0; x < grid[0].length; x++) {
        max = Math.max(
            max,
            shineLight(grid, { coord: { x, y: 0 }, dir: "S" }),
            shineLight(grid, { coord: { x, y: grid.length - 1 }, dir: "N" }))
    }

    for(let y = 0; y < grid.length; y++) {
        max = Math.max(
            max,
            shineLight(grid, { coord: { x: 0, y }, dir: "E" }),
            shineLight(grid, { coord: { x: grid[0].length - 1, y }, dir: "W" }))
    }

    return max
}

export const Day16 = {
    number: 16,
    title: "The Floor Will Be Lava",
    parseInput,
    part1,
    part2,
}
