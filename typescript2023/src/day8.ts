import { gcd, lcm, mult } from "./shared"

type Node = {
    name: string
    left: string
    right: string
}

type Map = {
    instructions: string[],
    nodes: Record<string, Node>
}

function parseInput(input: string): Map {
    const [instructions, nodes] = input.split("\n\n")

    return {
        instructions: instructions.split(""),
        nodes: nodes
            .split("\n")
            .map((line) => {
                const [name, lr] = line.split(" = ");
                const [left, right] = lr.slice(1, -1).split(", ");
                return { name, left, right };
            })
            .reduce((acc: Record<string, Node>, cur) => {
                acc[cur.name] = cur;
                return acc;
            }, {})
    }
}

function walkMap(map: Map, from: string, done: (_: string) => boolean): number {
    let nSteps = 0;
    let cur = from
    while(!done(cur)) {
        switch(map.instructions[nSteps % map.instructions.length]) {
            case "L":
                cur = map.nodes[cur].left
                break
            case "R":
                cur = map.nodes[cur].right
                break
        }
        nSteps += 1
    }
    return nSteps
}

function part1(map: Map): number {
    return walkMap(map, "AAA", (name: string) => name.endsWith("Z"))
}

function part2(map: Map): number {
    const starts = Object.keys(map.nodes).filter((name) => name.endsWith("A"))
    const distances = starts.map((start) => walkMap(map, start, (name: string) => name.endsWith("Z")))
    return distances.reduce(lcm)
}

export const Day8 = {
    number: 8,
    title: "Haunted Wasteland",
    parseInput,
    part1,
    part2,
}
