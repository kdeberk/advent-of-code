#!/usr/bin/env ts-node

// Day 2: Cube Conundrum
// Part 1: Find the 'cube reveals' that would have been possible if the bag contained only 12 red, 13 green and 14 blue cubes.
// Part 2: For each game session, get the least number of cubes needed to make the reveals possible.

type Color = "red" | "blue" | "green"
const COLORS: Color[] = ["red", "blue", "green"]

type Game = {
    id: number
    sets: Record<Color, number>[]
}

function parseInput(input: string): Game[] {
    return input
        .split("\n")
        .map((line) => {
            const [pre, after] = line.split(": ")

            return {
                id: parseInt(pre.split(" ")[1], 10),
                sets: after.split("; ").map((frag) => {
                    return frag.split(", ")
                        .map((x) => x.split(" "))
                        .reduce((acc: Record<string, number>, [n, color]) => {
                            acc[color] = parseInt(n, 10)
                            return acc
                        }, {red: 0, green: 0, blue: 0})
                })
            }
        })
}

const sum = (a: number, b: number) => a + b
const product = (a: number, b: number) => a * b

function part1(games: Game[]): number {
    const MAX: Record<Color, number> = {red: 12, green: 13, blue: 14}

    return games
        .filter((game) => game.sets.every((set) => COLORS.every((color) => set[color] <= MAX[color])))
        .map((game) => game.id)
        .reduce(sum)
}

function part2(games: Game[]): number {
    return games
        .map((game) =>
            COLORS.map((color) => Math.max(...game.sets.map((set) => set[color])))
                .reduce(product)
            )
        .reduce(sum)
}

export const Day2 = {
    number: 2,
    title: "Cube Conundrum",
    parseInput,
    part1,
    part2,
}
