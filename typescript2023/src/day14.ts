type Coord = { x: number, y: number }

type Platform = {
    width: number,
    height: number,
    cubeRocks: Record<string, Coord>
    roundRocks: Record<string, Coord>
}

function coordKey(c: Coord): string {
    return `${c.x}-${c.y}`
}

function parseInput(input: string): Platform {
    const grid = input.split("\n").map(line => line.split(""))

    const roundRocks:Record<string, Coord> = {}
    const cubeRocks:Record<string, Coord> = {}
    for(let y = 0; y < grid.length; y++) {
        for(let x = 0; x < grid[0].length; x++) {
            if(grid[y][x] === 'O') {
                roundRocks[coordKey({ x, y })] = { x, y }
            } else if(grid[y][x] == "#") {
                cubeRocks[coordKey({ x, y })] = { x, y }
            }
        }
    }

    return { cubeRocks, roundRocks, width: grid[0].length, height: grid.length }
}

function roll(platform: Platform, dx: number, dy: number): Platform {
    let done = false
    while(!done) {
        done = true
        for(const round of Object.values(platform.roundRocks)) {
            const newX = round.x + dx
            const newY = round.y + dy
            if(!(0 <= newX && newX < platform.width && 0 <= newY && newY < platform.height)) {
                continue
            }

            const oldKey = coordKey({ x: round.x, y: round.y })
            const newKey = coordKey({ x: newX, y: newY })
            if(!(newKey in platform.cubeRocks) && !(newKey in platform.roundRocks)) {
                delete platform.roundRocks[oldKey]
                platform.roundRocks[newKey] = { x: newX, y: newY }
                done = false
            }
        }
    }
    return platform
}

function platformScore(platform: Platform): number {
    let score = 0;
    for (const round of Object.values(platform.roundRocks)) {
        score += platform.height - round.y
    }

    return score
}

function part1(platform: Platform): number {
    platform = roll(platform, 0, -1)
    return platformScore(platform)
}

function part2(platform: Platform): number {
    function cycle(platform: Platform): Platform {
        for(const [dx, dy] of [[0, -1], [-1, 0], [0, 1], [1, 0]]) {
            platform = roll(platform, dx, dy)
        }
        return platform
    }

    const seen: Record<string, number> = {}
    for(let idx = 0; idx < 1_000_000_000; idx++) {
        platform = cycle(platform)
        const k = [...Object.keys(platform.roundRocks)].sort().join("#")
        if(k in seen) {
            const d = idx - seen[k]
            console.log(`Saw this position before: now: ${idx}, then: ${seen[k]}`)
            while(idx + d < 1_000_000_000) {
                idx += d
            }
            console.log(idx, d)
        } else {
            seen[k] = idx
        }
    }

    return platformScore(platform)
}

export const Day14 = {
    number: 14,
    title: "Parabolic Reflector Dish",
    parseInput,
    part1,
    part2,
}
