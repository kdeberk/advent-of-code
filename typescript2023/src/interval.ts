export class Interval {
    public readonly length: number;

    constructor(public start: number, public end: number) {
        this.length = end - start + 1
    }

    static nonOverlapping(is: Interval[]): Interval[] {
        return this.unions(this.walk(is, (x) => x === 1))
    }

    static intersection(is: Interval[]): Interval[] {
        return this.walk(is, (x) => 1 < x)
    }

    static unions(is: Interval[]): Interval[] {
        return this.walk(is, (x) => 1 <= x)
    }

    private static walk(is: Interval[], startFn: (_: number) => boolean) {
        const res:Interval[] = []

        const events = is.map((i) => [i.start, 1])
        events.push(...is.map((i) => [i.end, -1]))
        events.sort((a, b) => a[0] - b[0])

        let x = 0
        let curStart: number | undefined;
        for(const [value, dx] of events) {
            x += dx
            if(startFn(x)) {
                if(curStart === undefined) {
                    curStart = value
                }
            } else if(curStart !== undefined) {
                if(curStart !== value) {
                    res.push(new Interval(curStart, value))
                }
                curStart = undefined
            }
        }

        return res

    }
}
