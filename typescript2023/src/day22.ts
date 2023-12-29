import { add } from "./shared"

type Coord = {
    x: number,
    y: number,
    z: number,
}

type Brick = {
    idx: number,
    a: Coord,
    b: Coord,

    minX: number,
    maxX: number,
    minY: number,
    maxY: number,
    minZ: number,
    maxZ: number,
    height: number,
    safe?: boolean,
}

function newBrick(idx: number, a: Coord, b: Coord): Brick {
    const minX = Math.min(a.x, b.x)
    const maxX = Math.max(a.x, b.x)
    const minY = Math.min(a.y, b.y)
    const maxY = Math.max(a.y, b.y)
    const minZ = Math.min(a.z, b.z)
    const maxZ = Math.max(a.z, b.z)

    return {
        a, b, idx,
        minX, maxX, minY, maxY, minZ, maxZ,
        height: (maxZ - minZ + 1),
    }
}

type Input = {
    bricks: Brick[],
    supporting:  Record<number, Set<number>>,
    supportedBy: Record<number, Set<number>>,
}

function parseInput(input: string): Input {
    function parseCoord(str: string): Coord {
        const [x, y, z] = str.split(",").map((n) => parseInt(n))
        return { x, y, z }
    }

    function parseBrick(str: string, idx: number): Brick {
        const [a, b] = str.split("~").map(parseCoord)
        return newBrick(idx, a, b)
    }

    function settleBricks(bricks: Brick[]): Brick[] {
        const settled: Brick[] = []
        const ground: Record<string, number> = {}

        bricks.sort((a: Brick, b: Brick): number => a.minZ - b.minZ)

        for (const brick of bricks) {
            let maxHeight = 0
            for (let x = brick.minX; x <= brick.maxX; x++) {
                for (let y = brick.minY; y <= brick.maxY; y++) {
                    const k = `${x}-${y}`
                    if (k in ground && maxHeight < ground[k]) {
                        maxHeight = ground[k]
                    }
                }
            }

            for (let x = brick.minX; x <= brick.maxX; x++) {
                for (let y = brick.minY; y <= brick.maxY; y++) {
                    ground[`${x}-${y}`] = maxHeight + brick.height
                }
            }

            const distFallen = brick.minZ - maxHeight
            settled.push(newBrick(
                brick.idx,
                { x: brick.a.x, y: brick.a.y, z: brick.a.z - distFallen },
                { x: brick.b.x, y: brick.b.y, z: brick.b.z - distFallen },
            ))
        }

        return settled
    }

    const bricks = settleBricks(input.split("\n").map(parseBrick))

    const layout: Record<string, number> = {}
    for (const brick of bricks) {
        for (let x = brick.minX; x <= brick.maxX; x++) {
            for (let y = brick.minY; y <= brick.maxY; y++) {
                layout[`${x}-${y}-${brick.minZ}`] = brick.idx
            }
        }
    }

    const supporting: Record<number, Set<number>> = {}
    const supportedBy: Record<number, Set<number>> = {}

    for (const brick of bricks) {
        supportedBy[brick.idx] = new Set()
        supporting[brick.idx] = new Set()
    }

    for (const brick of bricks) {
        for (let x = brick.minX; x <= brick.maxX; x++) {
            for (let y = brick.minY; y <= brick.maxY; y++) {
                const k = `${x}-${y}-${brick.maxZ + 1}`

                if (!(k in layout)) {
                    continue
                }
                supportedBy[layout[k]].add(brick.idx)
                supporting[brick.idx].add(layout[k])
            }
        }
    }

    return { bricks, supporting, supportedBy }
}

function countSafeBricks(input: Input): number {
    for(const brick of input.bricks) {
        if(!(brick.idx in input.supporting)) {
            brick.safe = true
            continue
        }

        brick.safe = true
        for(const other of input.supporting[brick.idx]) {
            if(input.supportedBy[other].size <= 1) {
                brick.safe = false
                break
            }
        }
    }

    return input.bricks.filter((brick) => brick.safe).length
}


function part1(input: Input): number {
    return countSafeBricks(input)
}

function part2(input: Input): number {
    console.log(input.supporting)

    const byIdx: Record<number, Brick> = {}
    for(const brick of input.bricks) {
        byIdx[brick.idx] = brick
    }

    for (let idx = input.bricks.length - 1; 0 <= idx; idx--) {
        const brick = input.bricks[idx]

        for (const supporting of input.supportedBy[brick.idx]) {
            for(const supported of input.supporting[brick.idx]) {
                input.supporting[supporting].add(supported)
            }
        }
    }

    let count = 0
    for(const brick of input.bricks) {
        const supporteds:Brick[] = []
        for(const supportedIdx of input.supporting[brick.idx]) {
            supporteds.push(byIdx[supportedIdx])
        }
        supporteds.sort((a, b) => a.minZ - b.minZ)

        const wontFall = new Set();
        for(const supported of supporteds) {
            let willFall = true
            for(const otherSupport of input.supportedBy[supported.idx]) {
                if(otherSupport === brick.idx) {
                    continue
                }

                if(wontFall.has(otherSupport) || !input.supporting[brick.idx].has(otherSupport)) {
                    willFall = false
                    wontFall.add(supported.idx)
                    break
                }
            }

            if(willFall) {
                count++
            }
        }
    }

    return count
}

export const Day22 = {
    number: 22,
    title: "Sand Slabs",
    parseInput,
    part1,
    part2,
}

// 4620: Too low
// 143788: Too high

// Wrong: 71459
