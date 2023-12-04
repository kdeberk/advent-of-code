#!/usr/bin/env ts-node

import { readFileSync } from "fs";

import { Day1 } from "./day1";
import { Day2 } from "./day2";
import { Day3 } from "./day3";
import { Day4 } from "./day4";

export type Day<I = any, R = any> = {
    number: number,
    parseInput: (_: string) => I,
    part1: (_: I) => R
    part2: (_: I) => R
}

const Days = [Day1, Day2]

export function runDay(day: Day): void {
    const input = readFileSync(__dirname + `/../input/day${day.number}.txt`, "utf-8");
    const parsed = day.parseInput(input);
    console.log(day.part1(parsed), day.part2(parsed))
}

runDay(Day1)
runDay(Day2)
runDay(Day3)
runDay(Day4)
