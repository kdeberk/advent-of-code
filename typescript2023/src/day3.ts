type Coord = { x: number, y: number }

// Day 3: Gear Ratios
// Part 1: Identify the digit sequences on the schematic that are adjacent to an engine component
// Part 2: Identify the digit sequences that are adjacent to a gear.

function neighbors(c: Coord): Coord[] {
    const neighbors:Coord[] = []
    for(let x = c.x - 1; x <= c.x + 1; x++) {
        for(let y = c.y - 1; y <= c.y + 1; y++) {
            if(x === c.x && y === c.y) {
                continue
            }
            neighbors.push({ x, y })
        }
    }
    return neighbors
}

type Part = { id: string, part: string, coord: Coord, numbers: number[] }

function parseInput(input: string): Part[] {
    type PartNumber = { id: string, value: number }

    const coordKey = (c: Coord) => `${c.x}:${c.y}`

    const parts: Part[] = []
    const partNumbers: Record<string, PartNumber> = {}

    // Read 2d grid cell by cell
    const lines = input.split("\n")
    for(let y = 0; y < lines.length; y++) {
        let n: PartNumber | null = null;

        for(let x = 0; x < lines[0].length; x++) {
            const c = lines[y][x]
            const key = coordKey({ x, y })

            if('0' <= c && c <= '9') {
                if(n === null) {
                    n = { id: key, value: 0 }
                }

                n.value = n.value * 10 + Number(c)
                partNumbers[key] = n
                continue
            }

            n = null;

            if('.' === c) {
                continue
            }

            parts.push({ id: key, part: c, coord: { x, y }, numbers: []})
        }
    }

    // For each found part, find the neighboring numbers.
    for(const part of parts) {
        const seen: Record<string, true> = {}

        for(const neighbor of neighbors(part.coord)) {
            const k = coordKey(neighbor)
            if(!(k in partNumbers)) {
                continue
            }

            const partNumber = partNumbers[k]
            if (partNumber.id in seen) {
                continue
            }

            part.numbers.push(partNumber.value)
            seen[partNumber.id] = true
        }
    }

    return parts;
}

function part1(parts: Part[]): number {
    let sum = 0;

    for(const part of parts) {
        for(const partNumber of part.numbers) {
            sum += partNumber
        }
    }

    return sum
}

function part2(parts: Part[]): number {
    let sum = 0

    for(const part of parts) {
        if(part.part === '*' && part.numbers.length === 2) {
            sum += part.numbers[0] * part.numbers[1]
        }
    }

    return sum
}

export const Day3 = {
    number: 3,
    title: "Gear Ratios",
    parseInput,
    part1,
    part2,
}
