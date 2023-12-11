
type Mapping = { start: number, end: number, add: number }
type Mappings = Record<string, [string, Mapping[]]>
type Almanac = {
    seeds: number[],
    mappings: Mappings,
}

function parseInput(input: string): Almanac {
    const seeds: number[] = []
    const mappings: Mappings = {}

    function parseFragment(frag: string): Mapping {
        const [to, from, len] = frag.split(" ").map((x) => parseInt(x))
        const add = to - from
        return { start: from, end: from + len, add }
    }

    function sort(mappings: Mapping[]): Mapping[] {
        mappings.sort((a: Mapping, b: Mapping): -1 | 1 => {
            if(a.start < b.start) {
                return -1
            } else if(a.start === b.start) {
                return a.end < b.end ? -1 : 1
            }
            return 1
        })
        return mappings
    }

    for(const block of input.split("\n\n")) {
        if(block.startsWith("seeds: ")) {
            seeds.push(...block.split(": ")[1].split(" ").map((x) => parseInt(x)))
            continue
        }

        const [header, ...rest] = block.split("\n")
        const [from, _, to] = header.split(" ")[0].split("-")
        mappings[from] = [to, sort(rest.map(parseFragment))]
    }

    return { seeds, mappings }
}

function part1(almanac: Almanac): number {
    function lowestLocation(almanac: Almanac, type: string, n: number): number {
        if (type === "location") {
            return n
        }

        let best: number | null = null
        const [dst, mappings] = almanac.mappings[type]
        for (const { start, end, add } of mappings) {
            if (start <= n && n < end) {
                const cand = lowestLocation(almanac, dst, n + add)
                if (best === null || cand < best) {
                    best = cand
                }
            }
        }

        if (best === null) {
            best = lowestLocation(almanac, dst, n)
        }

        return best
    }

    return Math.min(...almanac.seeds.map((seed) => lowestLocation(almanac, "seed", seed)))
}

function part2(almanac: Almanac): number {
    type Range = {start: number, end: number }

    let ranges: Range[] = []
    for(let idx = 0; idx < almanac.seeds.length; idx += 2) {
        ranges.push({ start: almanac.seeds[idx], end: almanac.seeds[idx] + almanac.seeds[idx+1] })
    }

    let type = "seed"
    while(type != "location") {
        const nxtRanges:Range[]  = [];
        const [nxtType, ms] = almanac.mappings[type]
        for (const range of ranges) {

            const overlaps: Mapping[] = []
            for(const m of ms) {
                if (range.end < m.start || m.end <= range.start) {
                    continue
                }
                overlaps.push(m)
            }

            if(overlaps.length === 0) {
                nxtRanges.push(range)
                continue
            }

            const first = overlaps[0]
            const last = overlaps[overlaps.length-1]


            let lastEnd: number | null = null
            if (range.start < first.start) {
                nxtRanges.push({ start: range.start, end: overlaps[0].start })
            }

            for(const m of overlaps) {
                if(lastEnd && lastEnd < m.start) {
                    nxtRanges.push({ start: lastEnd, end: m.start })
                }

                nxtRanges.push({ start: Math.max(m.start, range.start) + m.add, end: Math.min(m.end, range.end) + m.add })
            }

            if (last.end < range.end) {
                nxtRanges.push({ start: last.end, end: range.end })
            }
        }
        type = nxtType
        ranges = nxtRanges
    }

    let lowest = Number.MAX_VALUE
    for(const range of ranges) {
        if(range.start < lowest) {
            lowest = range.start
        }
    }
    return lowest
}

export const Day5 = {
    number: 5,
    parseInput,
    part1,
    part2,
}
