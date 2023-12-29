import { MinHeap } from "./shared"

type Coord = { x: number, y: number }
type PathNode = { key: string, coord: Coord }

type Forest = {
    start: PathNode,
    end: PathNode,
    grid: string[][],
    nodes: Record<string, PathNode>,
}

function coordKey(c: Coord): string {
    return `${c.x}-${c.y}`
}

function parseInput(input: string): Forest {
    const grid: string[][] = input.split("\n").map((line) => line.split(""))
    const start = { key: "start", coord: { x: 1, y: 0 }, }
    const end = { key: "end", coord: { x: grid[0].length - 2, y: grid.length - 1 } }

    function findPathNodes(): Record<string, PathNode> {
        const nodes: Record<string, PathNode> = {
            [coordKey(start.coord)]: start,
            [coordKey(end.coord)]: end,
        }

        for(let y = 1; y < grid.length - 1; y++) {
            for(let x = 1; x < grid[0].length - 1; x++) {
                if(grid[y][x] === "#") {
                    continue
                }

                let countPaths = 0
                for(const [dx, dy] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
                    if(grid[y+dy][x+dx] !== "#") {
                        countPaths += 1
                    }
                }

                if(2 < countPaths) {
                    nodes[`${x}-${y}`] = { coord: { x, y }, key: `${Object.keys(nodes).length}` }
                }
            }
        }
        return nodes
    }

    return { start, end, grid, nodes: findPathNodes() }
}

function getDistances(forest: Forest, slippery: boolean): Record<string, Record<string, number>> {
    const { nodes, start, grid } = forest

    const distances: Record<string, Record<string, number>> = {}
    for (const node of Object.values(nodes)) {
        distances[node.key] = {}
    }

    const q: { pos: Coord, d: number, prevNode: string, out: boolean, in_: boolean }[] = [
        { pos: start.coord, d: 0, prevNode: start.key, out: true, in_: true },
    ]
    const seen: Set<string> = new Set()
    while (0 < q.length) {
        let { pos, d, prevNode, out, in_ } = q.shift()!

        if (seen.has(`${pos.x}-${pos.y}-${prevNode}`)) {
            continue
        }
        seen.add(`${pos.x}-${pos.y}-${prevNode}`)

        const k = `${pos.x}-${pos.y}`
        if (k in nodes && nodes[k].key != prevNode) {
            if (!(prevNode in distances)) {
                distances[prevNode] = {}
            }
            if (!(nodes[k].key in distances)) {
                distances[nodes[k].key] = {}
            }

            if (out || !slippery) {
                distances[prevNode][nodes[k].key] = d
            }
            if (in_ || !slippery) {
                distances[nodes[k].key][prevNode] = d
            }
            prevNode = nodes[k].key
            out = true
            in_ = true
            d = 0
        }

        for (const [dx, dy] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
            const x = pos.x + dx
            const y = pos.y + dy
            if (!(0 <= x && x < grid[0].length && 0 <= y && y < grid.length)) {
                continue
            }
            if (grid[y][x] === "#") {
                continue
            } else if (grid[y][x] === ">" && dx === -1) {
                q.push({ pos: { x, y }, d: d + 1, prevNode, out: false, in_ })
            } else if (grid[y][x] === ">" && dx === 1) {
                q.push({ pos: { x, y }, d: d + 1, prevNode, out, in_: false })
            } else if (grid[y][x] === "v" && dy === -1) {
                q.push({ pos: { x, y }, d: d + 1, prevNode, out: false, in_ })
            } else if (grid[y][x] === "v" && dy === 1) {
                q.push({ pos: { x, y }, d: d + 1, prevNode, out, in_: false })
            } else {
                q.push({ pos: { x, y }, d: d + 1, prevNode, out, in_ })
            }
        }
    }

    return distances
}

function printGrid(forest: Forest) {
    const { grid } = forest;
    for(let y = 0; y < grid.length; y++) {
        for(let x = 0; x < grid[0].length; x++) {
            if(["#", ">", "v"].includes(grid[y][x])) {
                process.stdout.write(grid[y][x])
            } else if(`${x}-${y}` in forest.nodes) {
                process.stdout.write(forest.nodes[`${x}-${y}`].key)
            } else {
                process.stdout.write(' ')
            }
        }
        console.log("")
    }
}

function part1(forest: Forest): number {
    const distances = getDistances(forest, true)

    const q:{ cur: string, d: number}[] = [{cur: "start", d: 0}]

    let max = 0;
    while(0 < q.length) {
        const { cur, d } = q.shift()!

        if(cur === "end") {
            max = Math.max(d, max)
            continue
        }

        for(const [nxt, dx] of Object.entries(distances[cur])) {
            q.push({ cur: nxt, d: d+dx })
        }
    }

    return max
}

function part2(forest: Forest): number {
    const distances = getDistances(forest, false)

    type SearchNode = {
        cur: string
        d: number
        seen: Set<string> // TODO: figure out why bitwise masking doesn't work
    }

    const ids: Record<string, number> = {}
    for(const node of Object.values(forest.nodes)) {
        ids[node.key] = 2**(Object.keys(ids).length)
    }
    console.log(ids)

    let max = 0;
    const heap = new MinHeap<SearchNode>((x) => 0 - x.d)
    heap.push({ cur: "start", d: 0, seen: new Set() })

    while(heap.peek()) {
        const { cur, d, seen } = heap.pop()!

        if(cur === "end") {
            if(max < d) {
                console.log("new max", d)
            }
            max = Math.max(d, max)
            continue
        }

        // if(0 < (ids[cur] & seen)) {
        //     continue
        // }
        if(seen.has(cur)) {
            continue
        }

        for(const [nxt, dx] of Object.entries(distances[cur])) {
            // if(0 < (ids[nxt] & seen)) {
            //     continue
            // }
            if(seen.has(nxt)) {
                continue
            }
            heap.push({ cur: nxt, d: d+dx, seen: new Set([...seen, cur ])})
        }
    }

    return max
}

export const Day23 = {
    number: 23,
    title: "A Long Walk",
    parseInput,
    part1,
    part2,
}

// Too low: 6000
// Wrong: 6706
