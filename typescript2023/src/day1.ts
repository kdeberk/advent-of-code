#!/usr/bin/env ts-node

import { readFileSync } from "fs";

function firstToAppear(str: string, items: string[]): string {
    let bestIdx = str.length
    let best: string | null = null
    for(const cur of items) {
        const idx = str.indexOf(cur)
        if(-1 < idx && idx < bestIdx) {
            best = cur;
            bestIdx = idx
        }
    }
    return best!
}

function lastToAppear(str: string, items: string[]): string {
    let bestIdx = -1
    let best: string | null = null
    for(const cur of items) {
        const idx = str.lastIndexOf(cur)
        if(-1 < idx && bestIdx < idx) {
            best = cur;
            bestIdx = idx
        }
    }
    return best!
}

function part1(lines: string[]): number {
    const KEYS = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

    let sum = 0;
    for (const line of lines) {
        const first = parseInt(firstToAppear(line, KEYS))
        const last = parseInt(lastToAppear(line, KEYS))

        sum += 10 * first + last
    }
    return sum
}

function part2(lines: string[]): number {
    const DIGIT_KEYS = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    const WORD_KEYS = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    const KEYS = DIGIT_KEYS.concat(WORD_KEYS)

    function parseKey(key: string) {
        if(key.length === 1) {
            return parseInt(key, 10)
        }
        return WORD_KEYS.indexOf(key) + 1
    }

    let sum = 0;
    for (const line of lines) {
        const first = parseKey(firstToAppear(line, KEYS))
        const last = parseKey(lastToAppear(line, KEYS))

        sum += 10 * first + last
    }
    return sum
}

function day1() {
    const input = readFileSync(__dirname + "/../input/day1.txt", "utf-8");
    const lines = input.split("\n").slice(0, -1)
    console.log(part1(lines), part2(lines))
}

day1();
