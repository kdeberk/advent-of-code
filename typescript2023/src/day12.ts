import { add } from "./shared"

type ConditionRecord = {
    record: string
    groups: number[]
}

function parseInput(input: string): ConditionRecord[] {
    return input
        .split("\n")
        .map((line) => {
            const [record, groups] = line.split(" ")
            return { record, groups: groups.split(",").map((x) => parseInt(x)) }
        })
}

function countArrangements(a: ConditionRecord): number {
    const cache: Record<string, number> = {}

    function inner(recordIdx: number, groupIdx: number, curGroup: number | null): number {
        const k = `${recordIdx}-${groupIdx}-${curGroup}`
        if(k in cache) {
            return cache[k]
        }

        if(recordIdx === a.record.length) {
            return groupIdx === a.groups.length && (curGroup === null || curGroup === 0) ? 1 : 0
        }

        let count: number = 0;

        const ch = a.record[recordIdx];
        if(ch === "?") {
            if(curGroup === null) {
                count += inner(recordIdx + 1, groupIdx, null) // Assume '.'
                count += inner(recordIdx + 1, groupIdx + 1, a.groups[groupIdx] - 1) // Assume '#' and start group
            } else if(0 < curGroup) {
                count = inner(recordIdx + 1, groupIdx, curGroup - 1) // Assume '#' and continue group
            } else {
                count = inner(recordIdx + 1, groupIdx, null) // Assume '.' and leave group
            }
        } else if(ch === '#') {
            if(curGroup === 0) { // Abort, no group was expected
                count = 0;
            } else if(curGroup === null) {
                count = inner(recordIdx + 1, groupIdx + 1, a.groups[groupIdx] - 1) // Start group
            } else {
                count = inner(recordIdx + 1, groupIdx, curGroup - 1) // Continue group
            }
        } else { // '.'
            if(curGroup !== null && 0 < curGroup) {
                count = 0; // Group was expected
            } else {
                count = inner(recordIdx + 1, groupIdx, null)
            }
        }

        cache[k] = count;
        return count
    }

    return inner(0, 0, null)
}

function part1(records: ConditionRecord[]): number {
    return records.map(countArrangements).reduce(add)
}

function part2(records: ConditionRecord[]): number {
    function expand(a: ConditionRecord): ConditionRecord {
        let records:string[] = [];
        let groups:number[] = []
        for(let idx = 0; idx < 5; idx++) {
            records.push(a.record);
            groups.push(...a.groups);
        }
        return { record: records.join("?"), groups }
    }

    return records.map(expand).map(countArrangements).reduce(add)
}

export const Day12 = {
    number: 12,
    title: "Hot Springs",
    parseInput,
    part1,
    part2,
}
