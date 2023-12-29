type Graph = {
    nodes: Record<string, number>,
    edges: Record<string, Set<string>>,
}

function parseInput(input: string): Graph {
    const nodes: Record<string, number> = {}
    const edges: Record<string, Set<string>> = {}

    for(let line of input.split("\n")) {
        const [from, tos] = line.split(": ")

        nodes[from] = 1
        if (!(from in edges)) {
            edges[from] = new Set<string>()
        }

        for (const to of tos.split(" ")) {
            nodes[to] = 1

            if (!(to in edges)) {
                edges[to] = new Set<string>()
            }
            edges[to].add(from)
            edges[from].add(to)
        }
    }

    return { nodes, edges }
}

function randomInt(max: number): number {
    return Math.floor(max * Math.random())
}

function randomNodes(graph: Graph): [string, string] {
    const keys = Object.keys(graph.nodes)
    while(true) {
        const a = randomInt(keys.length - 1)
        const b = randomInt(keys.length - 1)
        if(a !== b) {
            return [keys[a], keys[b]]
        }
    }
}


function edmondKarps(graph: Graph): { flow: number, S: Set<string>, T: Set<string> } {
    const [s, t] = randomNodes(graph)

    const eflow: Record<string, number> = {}
    const ecap: Record<string, number> = {}
    const erev: Record<string, string> = {}
    const es: Record<string, string> = {}

    for(const [from, tos] of Object.entries(graph.edges)) {
        for(const to of tos) {
            eflow[`${from}-${to}`] = 0
            ecap[`${from}-${to}`] = 1
            erev[`${from}-${to}`] = `${to}-${from}`
            es[`${from}-${to}`] = from
        }
    }

    let flow = 0
    while(true) {
        const q: string[] = [s]
        const pred: Record<string, string | null> = {}
        for(const node of Object.keys(graph.nodes)) {
            pred[node] = null
        }

        while(0 < q.length && pred[t] === null) {
            const cur = q.shift()!;
            for(const to of graph.edges[cur]) {
                const e = `${cur}-${to}`
                if(pred[to] === null && to != s && eflow[e] < ecap[e]) {
                    pred[to] = e
                    q.push(to)
                }
            }
        }

        if (pred[t] === null) {
            break
        }

        let df = Number.MAX_VALUE
        for(let e = pred[t]; e !== null; e = pred[es[e]]) {
            df = Math.min(df, ecap[e] - eflow[e])
        }
        for(let e = pred[t]; e !== null; e = pred[es[e]]) {
            eflow[e] += df
            eflow[erev[e]] -= df
        }
        flow += df
    }

    const S: Set<string> = new Set()
    {
        const q: string[] = [s]
        while (0 < q.length) {
            const cur = q.shift()!

            for(const to of graph.edges[cur]) {
                if(eflow[`${cur}-${to}`] === 0 && !S.has(to)) {
                    S.add(to)
                    q.push(to)
                }
            }
        }
    }

    const T: Set<string> = new Set()
    {
        const q: string[] = [t]
        while (0 < q.length) {
            const cur = q.shift()!;

            for(const to of graph.edges[cur]) {
                if(!S.has(to) && !T.has(to)) {
                    T.add(to)
                    q.push(to)
                }
            }
        }
    }

    return { flow, S, T }
}

function part1(graph: Graph): number {
    while(true) {
        const { flow, S, T } = edmondKarps(graph)
        if(flow === 3) {
            return S.size * T.size
        }
    }
}

function part2(_: Graph): number {
    return 0
}

export const Day25 = {
    number: 25,
    title: "Snowverload",
    parseInput,
    part1,
    part2,
}
