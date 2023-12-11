import { intersect } from "./shared"

type Pipe = "-" | "|" | "J" | "L" | "7" | "F"
type CoordKey = string
type Maze = {
    grid: string[][]
    loop: Set<CoordKey>
}

class Coord {
    public key: string;
    constructor(public x: number, public y: number) {
        this.x = x;
        this.y = y;
        this.key = `${x}-${y}`;
    }

    north(): Coord { return new Coord(this.x, this.y - 1) }
    east(): Coord { return new Coord(this.x + 1, this.y) }
    south(): Coord { return new Coord(this.x, this.y + 1) }
    west(): Coord { return new Coord(this.x - 1, this.y) }
}

function parseInput(input: string): Maze {
    function findStartCell(grid: string[][]): Coord {
        for (let y = 0; y < grid.length; y++) {
            for (let x = 0; x < grid[y].length; x++) {
                if(grid[y][x] === "S") {
                    return new Coord(x, y)
                }
            }
        }
        throw "Not found"
    }

    function determineStartCell(start: Coord, grid: string[][]): Pipe {
        let possible = new Set<Pipe>(["|", "-", "J", "F", "7", "L"])

        const north = start.north();
        if(0 < north.y) {
            if(["7", "F", "|"].includes(grid[north.y][north.x])) {
                possible = intersect(possible, new Set(["|", "J", "L"]))
            }
        }
        const south = start.south();
        if(["L", "J", "|"].includes(grid[south.y][south.x])) {
            possible = intersect(possible, new Set(["|", "F", "7"]));
        }
        const west = start.west();
        if(["L", "F", "-"].includes(grid[west.y][west.x])) {
            possible = intersect(possible, new Set(["-", "J", "7"]));
        }
        const east = start.east();
        if(["J", "7", "-"].includes(grid[east.y][east.x])) {
            possible = intersect(possible, new Set(["-", "L", "F"]));
        }

        return Array.from(possible)[0];
    }

    function determineMainLoop(start: Coord, grid: string[][]): Set<CoordKey> {
        const seen: Record<CoordKey, Coord> = {}
        const q: Coord[] = [start]
        function push(...cs: Coord[]) {
            for(const c of cs) {
                if(!(c.key in seen)) {
                    q.push(c)
                }
            }
        }

        while (0 < q.length) {
            const cur = q.shift()!
            if (cur.key in seen) {
                continue
            }
            seen[cur.key] = cur;
            switch(grid[cur.y][cur.x]) {
                case "-": push(cur.east(), cur.west()); continue;
                case "|": push(cur.north(), cur.south()); continue;
                case "J": push(cur.west(), cur.north()); continue;
                case "7": push(cur.west(), cur.south()); continue;
                case "L": push(cur.east(), cur.north()); continue;
                case "F": push(cur.east(), cur.south()); continue;
            }
        }
        return new Set(Object.keys(seen))
    }

    const grid = input.split("\n").map((line) => line.split(""))
    const startCoord = findStartCell(grid);
    grid[startCoord.y][startCoord.x] = determineStartCell(startCoord, grid);

    const loop = determineMainLoop(startCoord!, grid);
    for(let y = 0; y < grid.length; y++) {
        for(let x = 0; x < grid[0].length; x++) {
            if(loop.has(`${x}-${y}`)) {
                continue
            }
            grid[y][x] = "."
        }
    }
    return { grid, loop }
}

function part1(maze: Maze): number {
    return maze.loop.size / 2;
}

function part2(maze: Maze): number {
    let count = 0;
    for(let y = 0; y < maze.grid.length; y++) {
        let inside = false;
        let startLoop: string | null = null;
        for(let x = 0; x < maze.grid[0].length; x++) {
            const cur = maze.grid[y][x]
            switch(cur) {
                case '-':
                    continue
                case '7':
                case 'J':
                    if(startLoop === (cur === '7' ? 'L' : 'F')) {
                        inside = !inside;
                    }
                    startLoop = null;
                    continue
                case 'L':
                case 'F':
                    startLoop = cur;
                    continue
                case '|':
                    inside = !inside;
                    continue
                case ".":
                    if(inside) {
                        count += 1;
                    }
                    continue;
            }
        }
    }

    return count
}

export const Day10 = {
    number: 10,
    parseInput,
    part1,
    part2,
}
