
enum Pulse {
    Low,
    High
}

interface Module {
    handleInput(src: string, pulse: Pulse): Pulse | null
    reset(): void
}

class FlipFlop implements Module {
    state = Pulse.Low

    constructor(public readonly name: string) {}

    handleInput(_: string, pulse: Pulse): Pulse | null {
        if(pulse === Pulse.High) {
            return null
        }
        this.state = (this.state === Pulse.High ? Pulse.Low : Pulse.High)
        return this.state
    }

    reset(): void {
        this.state = Pulse.Low
    }
}

class Conjunction implements Module {
    memory: Record<string, Pulse> = {}

    constructor(public readonly name: string, public readonly srcs: string[]) {
        this.reset()
    }

    handleInput(src: string, pulse: Pulse): Pulse | null {
        this.memory[src] = pulse
        if(Object.values(this.memory).every((p) => p === Pulse.High)) {
            return Pulse.Low
        }
        return Pulse.High
    }

    reset(): void {
        this.memory = this.srcs
            .reduce((acc, src) => {
                acc[src] = Pulse.Low
                return acc
            }, {} as Record<string, Pulse>)
    }
}

class BroadCaster implements Module {
    handleInput(_: string, pulse: Pulse): Pulse | null {
        return pulse
    }

    reset(): void {}
}

type System = {
    cables: Record<string, string[]>
    modules: Record<string, Module>,
}

function parseInput(input: string): System {
    const cables: Record<string, string[]> = {}
    const unprocessed: { name: string, op: string}[] = []

    const sources: Record<string, string[]> = {}

    for(const line of input.split("\n")) {
        const [pre, post] = line.split(" -> ")
        const dsts = post.split(", ")

        let name = pre
        let op = ""
        if(name[0] === "%" || name[0] === "&") {
            name = name.slice(1)
            op = pre[0]
        }

        cables[name] = dsts

        for(const dst of dsts) {
            if(!(dst in sources)) {
                sources[dst] = []
            }
            sources[dst].push(name)
        }
        unprocessed.push({name, op })
    }

    const modules: Record<string, Module> = {}
    for(const { name, op } of unprocessed) {
        if(name === "broadcaster") {
            modules[name] = new BroadCaster()
        } else if(op === "%") {
            modules[name] = new FlipFlop(name)
        } else {
            modules[name] = new Conjunction(name, sources[name])
        }
    }

    return { cables, modules }
}

function resetSystem(sys: System): void {
    for(const m of Object.values(sys.modules)) {
        m.reset()
    }
}

function pushButton(sys: System, start: string, inspect?: string, wanted?: Pulse): {lowCount: number, highCount: number, observed: Pulse | null} {
    let lowCount = 0
    let highCount = 0
    let observed: Pulse | null = null

    const q: { src: string, cur: string, pulse: Pulse}[] = [{ src: "button", cur: start, pulse: Pulse.Low }]
    while(0 < q.length) {
        const { src, cur, pulse } = q.shift()!

        if(pulse === Pulse.Low) {
            lowCount++
        } else {
            highCount++
        }

        if(inspect && cur === inspect && wanted === pulse) {
            observed = pulse
        }
        if(!(cur in sys.modules)) {
            continue
        }

        const p = sys.modules[cur].handleInput(src, pulse)

        for(const dst of sys.cables[cur]) {
            if(p !== null) {
                q.push({ src: cur, cur: dst, pulse: p })
            }

        }
    }

    return { lowCount, highCount, observed }
}

function part1(sys: System): number {
    let totalLowCount = 0
    let totalHighCount = 0

    resetSystem(sys)
    for(let nPushes = 0; nPushes < 1000; nPushes++) {
        const { lowCount, highCount } = pushButton(sys, "broadcaster")
        totalLowCount += lowCount
        totalHighCount += highCount
    }
    return totalLowCount * totalHighCount
}

function part2(sys: System): number {
    function countPushes(src: string, dst: string): number {
        resetSystem(sys)
        for(let nPushes = 0; ; nPushes++) {
            const { observed } = pushButton(sys, src, dst, Pulse.High)
            if(observed === Pulse.High) {
                return nPushes + 1
            }
        }
    }

    const nodes = ["rg", "mr", "xr", "sv"]
    return nodes.map((node) => countPushes(node, "kz")).reduce((a, b) => a * b)
}

export const Day20 = {
    number: 20,
    title: "Pulse Propagation",
    parseInput,
    part1,
    part2,
}
