#!/usr/bin/env ts-node

import { readFileSync } from "fs";

import { Day1 } from "./day1";
import { Day2 } from "./day2";
import { Day3 } from "./day3";
import { Day4 } from "./day4";
import { Day5 } from "./day5";
import { Day6 } from "./day6";
import { Day7 } from "./day7";
import { Day8 } from "./day8";
import { Day9 } from "./day9";
import { Day10 } from "./day10";
import { Day11 } from "./day11";
import { Day12 } from "./day12";
import { Day13 } from "./day13";
import { Day14 } from "./day14";
import { Day15 } from "./day15";
import { Day16 } from "./day16";
import { Day17 } from "./day17";
import { Day18 } from "./day18";
import { Day19 } from "./day19";
import { Day20 } from "./day20";
import { Day21 } from "./day21";
import { Day22 } from "./day22";
import { Day23 } from "./day23";
import { Day24 } from "./day24";
import { Day25 } from "./day25";

export type Day<I = any, R = any> = {
    number: number,
    title: string
    parseInput: (_: string) => I,
    part1: (_: I) => R
    part2: (_: I) => R
}

const Days: Record<number, Day> = {}

for(const day of [Day25]) {//Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18]) {
    Days[day.number] = day
}

type Config = {
    days: Day[],
    test: boolean
}

function loadConfig(): Config {
    return {
        days: Object.values(Days),
        test: false,
    }
}

export function runDay(cfg: Config, day: Day): void {
    const filename = __dirname + `/../input/day${day.number}${cfg.test ? "_test" : ""}.txt`
    const input = readFileSync(filename, "utf-8");
    const parsed = day.parseInput(input.slice(0, -1)); // strip last newline

    console.log(`== Day ${day.number}: ${day.title}`)
    console.log("Part 1:", day.part1(parsed))
    console.log("Part 2:", day.part2(parsed))
}

export function main() {
    const cfg = loadConfig();
    for(const day of cfg.days) {
        runDay(cfg, day)
    }
}

main()
