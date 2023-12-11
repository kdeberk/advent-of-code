#!/usr/bin/env ts-node

import { readFileSync } from "fs";

// import { Day1 } from "./day1";
// import { Day2 } from "./day2";
// import { Day3 } from "./day3";
// import { Day4 } from "./day4";
// import { Day5 } from "./day5";
// import { Day6 } from "./day6";
// import { Day7 } from "./day7";
// import { Day8 } from "./day8";
//import { Day9 } from "./day9";
//import { Day10 } from "./day10";
import { Day11 } from "./day11";

export type Day<I = any, R = any> = {
    number: number,
    parseInput: (_: string) => I,
    part1: (_: I) => R
    part2: (_: I) => R
}

// const Days = [Day1, Day2]

export function runDay(day: Day): void {
    const input = readFileSync(__dirname + `/../input/day${day.number}.txt`, "utf-8");
    const parsed = day.parseInput(input.slice(0, -1)); // strip last newline
    console.log(day.part1(parsed), day.part2(parsed))
}

// runDay(Day1)
// runDay(Day2)
// runDay(Day3)
// runDay(Day4)
// runDay(Day5)
// runDay(Day6)
// runDay(Day7)
// runDay(Day8)
// runDay(Day9)
// runDay(Day10)
runDay(Day11)
