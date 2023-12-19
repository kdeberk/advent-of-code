type Galaxy = { x: number, y: number };
type Space = {
    galaxies: Set<Galaxy>,
    rowsWithGalaxies: Set<number>,
    colsWithGalaxies: Set<number>,
}

function parseInput(input: string): Space {
    const lines = input.split("\n")

    const rowsWithGalaxies = new Set<number>()
    const colsWithGalaxies = new Set<number>()
    const galaxies = new Set<Galaxy>();
    for(let y = 0; y < lines.length; y++) {
        for(let x = 0; x < lines[y].length; x++) {
            if(lines[y][x] === "#") {
                colsWithGalaxies.add(y)
                rowsWithGalaxies.add(x)
                galaxies.add({ x, y })
            }
        }
    }

    return { galaxies, rowsWithGalaxies, colsWithGalaxies }
}

function distance(a: Galaxy, b: Galaxy, space: Space, factor: number): number {
    let d = 0;
    for(let x = Math.min(a.x, b.x); x < Math.max(a.x, b.x); x++) {
        if(!space.rowsWithGalaxies.has(x)) {
            d += factor;
            continue;
        }
        d++;
    }
    for(let y = a.y; y < b.y; y++) {
        if(!space.colsWithGalaxies.has(y)) {
            d += factor;
            continue
        }
        d++;
    }
    return d;
}

function distanceAllPairs(space: Space, factor: number) {
    const galaxies = Array.from(space.galaxies);

    let sum = 0;
    for(let idx = 0; idx < galaxies.length; idx++) {
        for(let jdx = idx + 1; jdx < galaxies.length; jdx++) {
            sum += distance(galaxies[idx], galaxies[jdx], space, factor)
        }
    }

    return sum;
}


function part1(space: Space): number {
    return distanceAllPairs(space, 2)
}

function part2(space: Space): number {
    return distanceAllPairs(space, 1_000_000)
}

export const Day11 = {
    number: 11,
    title: "Cosmic Expansion",
    parseInput,
    part1,
    part2,
}
