import { mult } from "./shared"

type Race = {
    time: number
    distance: number
}

function parseInput(input: string): Race[] {
    const [times, distances] = input
        .split("\n")
        .map((line) => line.split(/\s+/))

    const races: Race[] = []
    for(let idx = 1; idx < times.length; idx++) {
        races.push({ time: parseInt(times[idx]), distance: parseInt(distances[idx]) })
    }
    return races
}

function countWinningStrategies(race: Race): number {
    let count = 0;
    for (let t = 0; t < race.time; t++) {
        const dist = (race.time - t) * t
        if (race.distance < dist) {
            count += 1
        }
    }
    return count
}

function part1(races: Race[]): number {
    return races.map(countWinningStrategies).reduce(mult, 1)
}

function part2(races: Race[]): number {
    const actualRace: Race = {
        time: parseInt(races.map((race) => race.time.toString()).join("")),
        distance: parseInt(races.map((race) => race.distance.toString()).join("")),
    }

    return countWinningStrategies(actualRace)
}

export const Day6 = {
    number: 6,
    title: "Wait For It",
    parseInput,
    part1,
    part2,
}
