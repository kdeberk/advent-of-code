import { add } from "./shared"

// import { Interval } from "./interval"

const MIN = 1
const MAX = 4000

type Part = {
    x: number,
    m: number
    a: number,
    s: number,
}

class Interval {
    public readonly length: number;

    constructor(readonly start: number, readonly end: number) {
        this.length = end - start + 1
    }

    contains(n: number): boolean {
        return this.start <= n && n <= this.end
    }

    intersect(other: Interval): Interval | null {
        if(!this.overlaps(other)) {
            return null
        }
        return new Interval(Math.max(this.start, other.start), Math.min(this.end, other.end))
    }

    disjoint(other: Interval): Interval {
        if(!this.overlaps(other)) {
            return this
        }

        if(this.start < other.start) {
            return new Interval(this.start, other.start-1)
        }
        return new Interval(other.end+1, this.end)
    }

    overlaps(other: Interval): boolean {
        if(other.start < this.start) {
            return other.overlaps(this)
        }
        return this.start <= other.start && other.start <= this.end ||
            this.start <= other.end && other.end <= this.end
    }
}


type Rule = {
    field: keyof Part
    interval: Interval,
    dst: string
}

type Workflow = {
    name: string
    rules: Rule[]
    default: string
}

type System = {
    parts: Part[],
    workflows: Record<string, Workflow>,
}

function processPart(sys: System, part: Part): "A" | "R" {
    let cur = "in"
    while(cur !== "A" && cur !== "R") {
        const workflow = sys.workflows[cur]

        let nxt: string | undefined;
        for(const rule of workflow.rules) {
            if(rule.interval.contains(part[rule.field])) {
                nxt = rule.dst
                break
            }
        }
        if(nxt === undefined) {
            nxt = workflow.default
        }

        cur = nxt!
    }
    return cur
}

function parseInput(input: string): System {
    function parseRule(line: string): Rule {
        const [pre, dst] = line.split(":")

        let opIdx = pre.indexOf("<")
        if(opIdx === -1) {
            opIdx = pre.indexOf(">")
        }
        const [param, op, val] = [pre.slice(0, opIdx), pre[opIdx], parseInt(pre.slice(opIdx+1), 10)]

        let interval = new Interval(val + 1, MAX)
        if(op === "<") {
            interval = new Interval(MIN, val - 1)
        }
        return { dst, interval, field: param as keyof Part }
    }

    function parseWorkflow(line: string): Workflow {
        const [name, allRules] = line.slice(0, line.length-1).split("{")
        const rules = allRules.split(",")

        return {
            name,
            rules: rules.slice(0, rules.length-1).map(parseRule),
            default: rules[rules.length-1],
        }
    }

    function parsePart(line: string): Part {
        return line.slice(1, line.length-1)
            .split(",")
            .map((frag: string) =>  {
                const [var_, val] = frag.split("=")
                return [var_, parseInt(val)] as [string, number]
            })
            .reduce((acc:Part, [var_, val]) => {
                acc[var_ as keyof Part] = val
                return acc
            }, {} as Part)
    }

    const [workflowLines, partLines] = input.split("\n\n")
    return {
        workflows: workflowLines
            .split("\n")
            .map(parseWorkflow)
            .reduce((acc, cur) => {
                acc[cur.name] = cur
                return acc
            }, {} as Record<string, Workflow>)
        ,
        parts: partLines.split("\n").map(parsePart),
    }
}

function part1(sys: System): number {
    let count = 0
    for(const part of sys.parts) {
        const outcome = processPart(sys, part)
        if(outcome === "A") {
            count += Object.values(part).reduce(add)
        }
    }
    return count
}

class PartSpace {
    readonly volume: number

    constructor(public x: Interval, public m: Interval, public a: Interval, public s: Interval) {
        this.volume = x.length * m.length * a.length * s.length
    }

    static All(): PartSpace {
        return new PartSpace(
            new Interval(MIN, MAX),
            new Interval(MIN, MAX),
            new Interval(MIN, MAX),
            new Interval(MIN, MAX),
        )
    }

    intersectDim(field: keyof Part, interval: Interval): PartSpace | null {
        let [x, m, a, s] = [this.x, this.m, this.a, this.s]

        switch(field) {
            case "x":
                x = this.x.intersect(interval)!
                break
            case "m":
                m = this.m.intersect(interval)!
                break
            case "a":
                a = this.a.intersect(interval)!
                break
            case "s":
                s = this.s.intersect(interval)!
                break
        }

        if(x === null || m === null || a === null || s === null) {
            return null
        }
        return new PartSpace(x, m, a, s)
    }

    disjoint(field: keyof Part, interval: Interval): PartSpace {
        let [x, m, a, s] = [this.x, this.m, this.a, this.s]

        switch(field) {
            case "x":
                x = x.disjoint(interval)
                break
            case "m":
                m = m.disjoint(interval)
                break
            case "a":
                a = a.disjoint(interval)
                break
            case "s":
                s = s.disjoint(interval)
                break
        }

        return new PartSpace(x, m, a, s)
    }

    intersect(other: PartSpace): PartSpace | null {
        const x = this.x.intersect(other.x)
        const m = this.m.intersect(other.m)
        const a = this.a.intersect(other.a)
        const s = this.s.intersect(other.s)

        if(x === null || m === null || a === null || s === null) {
            return null
        }
        return new PartSpace(x, m, a, s)
    }
}

function gatherSpaces(sys: System): PartSpace[] {
    const spaces: PartSpace[] = []
    const q: [string, PartSpace][] = [["in", PartSpace.All()]]

    while(0 < q.length) {
        let [cur, space] = q.shift()!
        if(cur === "A") {
            spaces.push(space)
            continue
        }
        if(cur === "R") {
            continue
        }

        const workflow = sys.workflows[cur]

        for(const rule of workflow.rules) {
            const intersect = space.intersectDim(rule.field, rule.interval)
            if(intersect !== null) {
                q.push([rule.dst, intersect])
            }
            space = space.disjoint(rule.field, rule.interval)
        }

        if(space !== null) {
            q.push([workflow.default, space])
        }
    }

    return spaces
}


function part2(sys: System): number {
    const spaces = gatherSpaces(sys)

    let volume = spaces.map((s) => s.volume).reduce(add)
    for(let idx = 0; idx < spaces.length; idx++) {
        for(let jdx = idx+1; jdx < spaces.length; jdx++) {
            const intersect = spaces[idx].intersect(spaces[jdx])
            if(intersect !== null) {
                volume -= intersect.volume
            }
        }
    }
    return volume
}

export const Day19 = {
    number: 19,
    title: "Aplenty",
    parseInput,
    part1,
    part2,
}
