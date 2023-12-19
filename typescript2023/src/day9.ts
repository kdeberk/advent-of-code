import { add, uniq } from "./shared";

function parseInput(input: string): number[][] {
    return input.split("\n")
        .map((line) => line.split(" ").map((n) => parseInt(n)))
}

function part1(seqs: number[][]): number {
    function next(seq: number[]): number {
        const diffs: number[] = []

        for(let idx = 1; idx < seq.length; idx++) {
            diffs.push(seq[idx] - seq[idx - 1])
        }

        return seq[seq.length - 1] + (uniq(diffs).length == 1 ? diffs[0] : next(diffs));
    }

    return seqs.map(next).reduce(add)
}

function part2(seqs: number[][]): number {
    function prev(seq: number[]): number {
        const diffs: number[] = []

        for(let idx = 1; idx < seq.length; idx++) {
            diffs.push(seq[idx] - seq[idx - 1])
        }

        return seq[0] - (uniq(diffs).length == 1 ? diffs[0] : prev(diffs));
    }

    return seqs.map(prev).reduce(add)
}

export const Day9 = {
    number: 9,
    title: "Mirage Maintenance",
    parseInput,
    part1,
    part2,
}
