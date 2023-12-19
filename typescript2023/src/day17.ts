import { MinHeap } from "./shared"

type City = number[][]
type Direction = "N" | "W" | "S" | "E"

function parseInput(input: string): City {
    return input.split("\n").map((line) => line.split("").map((n) => parseInt(n)))
}

type Crucible = {
    x: number,
    y: number,
    dir: Direction
    loss: number
    streak: number,
    minStreak: number,
    maxStreak: number,
}

function directions(dir: Direction): Direction[] {
    return {
        "N": ["N", "W", "E"],
        "S": ["S", "W", "E"],
        "E": ["E", "N", "S"],
        "W": ["W", "N", "S"],
    }[dir] as Direction[]
}

function move(c: Crucible, dir: Direction, city: City): Crucible | null {
    const [dx, dy] = {"N": [0, -1], "S": [0, 1], "E": [1, 0], "W": [-1, 0]}[dir]
    const x = c.x + dx
    const y = c.y + dy

    if(!(0 <= x && x < city[0].length && 0 <= y && y < city.length)) {
        return null
    }
    if(c.dir === dir && c.streak === c.maxStreak) {
        return null
    }
    if(c.dir !== dir && c.streak < c.minStreak) {
        return null
    }
    return { ...c, x, y, streak: c.dir === dir ? c.streak + 1 : 1, dir, loss: c.loss + city[y][x] }
}

function nextMoves(c: Crucible, city: City): Crucible[] {
    const nexts:Crucible[] = []
    for(const dir of directions(c.dir)) {
        const next = move(c, dir, city)
        if(next === null) {
            continue
        }

        nexts.push(next)
    }
    return nexts
}

function steerCrucible(city: City, c: Crucible): number {
    const heap = new MinHeap((c: Crucible) => {
        return c.loss
    })
    heap.push(c)

    function done(c: Crucible): boolean {
        return c.x === city[0].length-1 && c.y === city.length-1 && c.minStreak <= c.streak
    }

    const seen: Record<string, Crucible> = {}

    let min: number | undefined;
    while(heap.peek()) {
        const c = heap.pop()!
        const k = `${c.x}-${c.y}-${c.dir}-${c.streak}`
        if(k in seen && (seen[k].loss <= c.loss)) {
            continue
        }
        seen[k] = c

        if(done(c)) {
            return c.loss
        }

        for(const nxt of nextMoves(c, city)) {
            heap.push(nxt)
        }
    }

    return min!
}

function part1(city: City): number {
    return steerCrucible(city, { x: 0, y: 0, streak: 0, loss: 0, dir: "E", minStreak: 0, maxStreak: 3 })
}

function part2(city: City): number {
    return steerCrucible(city, { x: 0, y: 0, streak: 0, loss: 0, dir: "E", minStreak: 4, maxStreak: 10 })
}

export const Day17 = {
    number: 17,
    title: "Clumsy Crucible",
    parseInput,
    part1,
    part2,
}
