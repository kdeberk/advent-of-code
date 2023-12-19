import { add, rotateMatrix } from "./shared"

type Grid = string[][]

function parseInput(input: string): Grid[] {
    return input
        .split("\n\n")
        .map((block) => block.split("\n").map(line => line.split("")))
}

function findMirror(grid: Grid, maxSmudges: number): number {
    function findHorizontalMirror(grid: Grid): number | null {
        function isMirrorOnRow(mx: number): boolean {
            let nSmudges = 0;
            for (let lx = mx - 1, rx = mx; 0 <= lx && rx < grid[0].length; lx--, rx++) {
                for (let y = 0; y < grid.length; y++) {
                    if (grid[y][lx] != grid[y][rx]) {
                        nSmudges++
                        if (maxSmudges < nSmudges) {
                            return false;
                        }
                    }
                }
            }
            return nSmudges == maxSmudges
        }

        for (let mx = 1; mx < grid[0].length; mx++) {
            if (isMirrorOnRow(mx)) {
                return mx;
            }
        }
        return null
    }

    return findHorizontalMirror(grid) ?? 100 * findHorizontalMirror(rotateMatrix(grid))!;
}

function part1(grids: Grid[]): number {
    return grids.map((grid) => findMirror(grid, 0)).reduce(add)
}

function part2(grids: Grid[]): number {
    return grids.map((grid) => findMirror(grid, 1)).reduce(add)
}

export const Day13 = {
    number: 13,
    title: "Point of Incidence",
    parseInput,
    part1,
    part2,
}
