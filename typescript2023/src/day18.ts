import { Interval } from "./interval"
import { add } from "./shared"

type Direction = "R" | "L" | "D" | "U"
type Instruction = {
    dir: Direction,
    meters: number
    color: string
}

function parseInput(input: string): Instruction[] {
    return input.split("\n").map((line) => {
        const [dir, meters, color] = line.split(" ")
        return {
            dir: dir as Direction,
            meters: parseInt(meters),
            color: color.slice(1, color.length-1)
        }
    })
}

type Lagoon = {
    horBoundaries: [number, Interval[]][],
}

function digTrench(ins: Instruction[]): Lagoon {
    const lookup = {
        "R": [ 1,  0],
        "L": [-1,  0],
        "D": [ 0,  1],
        "U": [ 0, -1],
    }

    let [x, y] = [0, 0]

    const cornerCoords:Record<number, number[]> = {}
    for(const { dir, meters } of ins) {
        const [dx, dy] = lookup[dir]

        x += dx * meters
        y += dy * meters
        if(!(y in cornerCoords)) {
            cornerCoords[y] = []
        }
        cornerCoords[y].push(x)
    }

    const horBoundaries: [number, Interval[]][] = []
    for(const [y, xs] of Object.entries(cornerCoords)) {
        xs.sort((a, b) => a - b)

        const is: Interval[] = []
        for(let idx = 0; idx < xs.length; idx += 2) {
            is.push(new Interval(xs[idx], xs[idx+1]))
        }

        horBoundaries.push([parseInt(y, 10), is])
    }
    horBoundaries.sort((a, b) => a[0] - b[0])

    return { horBoundaries }
}

function calculateVolume(lagoon: Lagoon): number {
    function sumIntervals(ivs: Interval[]): number {
        return ivs.map((iv) => iv.length).reduce(add, 0)
    }

    let [prevY, upper] = lagoon.horBoundaries[0]

    let count = 0;
    for(const [y, cur] of lagoon.horBoundaries.slice(1)) {
        const nonOverlap = Interval.nonOverlapping(upper.concat(cur));
        const intersection = Interval.intersection(nonOverlap.concat(upper));
        count += (y - prevY + 1) * sumIntervals(upper) - sumIntervals(intersection)

        upper = nonOverlap
        prevY = y
    }

    return count
}

function part1(ins: Instruction[]): number {
    return calculateVolume(digTrench(ins));
}

function part2(ins: Instruction[]): number {
    function decodeInstruction(in_: Instruction): Instruction {
        const encDir = in_.color.slice(in_.color.length-1)
        const encMeters = in_.color.slice(1, in_.color.length-1)
        const dir = {"0": "R", "1": "D", "2": "L", "3": "U"}[encDir] as Direction

        let meters = parseInt(encMeters, 16)
        return { dir, meters, color: in_.color }
    }

    return calculateVolume(digTrench(ins.map(decodeInstruction)));
}

export const Day18 = {
    number: 18,
    title: "Lavaduct Lagoon",
    parseInput,
    part1,
    part2,
}
