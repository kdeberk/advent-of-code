class Rational {
    private readonly numerator: bigint
    private readonly denominator: bigint

    constructor(numerator: bigint, denominator: bigint = 1n) {
        if((numerator < 0n && denominator < 0n) || (denominator < 0n)) {
            numerator *= -1n
            denominator *= -1n
        }

        this.numerator = numerator
        this.denominator = denominator
    }

    add(o: Rational): Rational {
            return new Rational(this.numerator * o.denominator + o.numerator * this.denominator,
                                this.denominator * o.denominator)
    }

    sub(o: Rational): Rational {
            return new Rational(this.numerator * o.denominator - o.numerator * this.denominator,
                                this.denominator * o.denominator)
    }

    mul(o: Rational): Rational {
        return new Rational(this.numerator * o.numerator, this.denominator * o.denominator)
    }

    div(o: Rational): Rational {
        return new Rational(this.numerator * o.denominator, this.denominator * o.numerator)
    }

    less(o: Rational): boolean {
        const a = this.numerator * o.denominator
        const b = o.numerator * this.denominator
        return a < b
    }

    leq(o: Rational): boolean {
        const a = this.numerator * o.denominator
        const b = o.numerator * this.denominator
        return a <= b
    }

    isNegative(): boolean {
        return this.less(new Rational(0n))
    }

    isZero(): boolean {
        return this.numerator === 0n
    }

    asNumber(): number {
        if(this.isZero()) {
            return 0
        } else if(this.denominator === 1n) {
            return Number(this.numerator)
        }
        return Number(this.numerator) / Number(this.denominator)
    }
}

type Line = {
    p0: Vector,
    d: Vector,
}

class Vector {
    constructor(public readonly x: Rational, public readonly y: Rational, public readonly z: Rational) {}

    add(o: Vector): Vector {
        return new Vector(this.x.add(o.x), this.y.add(o.y), this.z.add(o.z))
    }

    sub(o: Vector): Vector {
        return new Vector(this.x.sub(o.x), this.y.sub(o.y), this.z.sub(o.z))
    }

    mul(s: Rational): Vector {
        return new Vector(this.x.mul(s), this.y.mul(s), this.z.mul(s))
    }

    div(s: Rational): Vector {
        return new Vector(this.x.div(s), this.y.div(s), this.z.div(s))
    }
}

function cross(a: Vector, b: Vector): Vector {
    return new Vector(
        a.y.mul(b.z).sub(a.z.mul(b.y)),
        a.z.mul(b.x).sub(a.x.mul(b.z)),
        a.x.mul(b.y).sub(a.y.mul(b.x)),
    )
}

function dot(a: Vector, b: Vector): Rational {
    const x = a.x.mul(b.x)
    const y = a.y.mul(b.y)
    const z = a.z.mul(b.z)
    return x.add(y).add(z)
}

function parseInput(input: string): Line[] {
    return input
        .split("\n")
        .map((line) => {
            const [[x0, y0, z0], [dx, dy, dz]] = line
                .split(" @ ")
                .map((frag) => frag.split(", ").map((n) => new Rational(BigInt(parseInt(n)))))
            return { p0: new Vector(x0, y0, z0), d: new Vector(dx, dy, dz) }
        })
}

type Coord2D = {
    x: Rational,
    y: Rational,
}

function intersection2D(a: Line, b: Line): Coord2D | undefined {
    const [x1, y1] = [a.p0.x, a.p0.y]
    const [x2, y2] = [a.p0.x.add(a.d.x), a.p0.y.add(a.d.y)]
    const [x3, y3] = [b.p0.x, b.p0.y]
    const [x4, y4] = [b.p0.x.add(b.d.x), b.p0.y.add(b.d.y)]

    const div = (x1.sub(x2)).mul(y3.sub(y4))
        .sub((y1.sub(y2)).mul(x3.sub(x4)))
    if(div.isZero()) {
        return undefined
    }

    const f1 = (x1.mul(y2).sub(y1.mul(x2))).div(div)
    const f2 = (x3.mul(y4).sub(y3.mul(x4))).div(div)

    return {
        x: (f1.mul(x3.sub(x4))).sub(f2.mul((x1.sub(x2)))),
        y: (f1.mul(y3.sub(y4))).sub(f2.mul((y1.sub(y2)))),
    }
}

function part1(lines: Line[]): number {
    let count = 0
    let min = new Rational(200000000000000n) // 7n
    let max = new Rational(400000000000000n) // 27n
    for(let idx = 0; idx < lines.length; idx++) {
        for(let jdx = idx+1; jdx < lines.length; jdx++) {
            const [a, b] = [lines[idx], lines[jdx]]

            const p = intersection2D(a, b)
            if(!p) {
                continue
            }

            const ta = p.x.sub(a.p0.x).div(a.d.x)
            const tb = p.x.sub(b.p0.x).div(b.d.x)
            if(ta.isNegative() || tb.isNegative()) {
                continue
            }

            if(min.leq(p.x) && p.x.leq(max) && min.leq(p.y) && p.y.leq(max)) {
                count++
            }
        }
    }

    return count
}

function normalizeToLine0(lines: Line[]): Line[] {
    return lines.map((line: Line) => {
        return {
            p0: line.p0.sub(lines[0].p0),
            d: line.d.sub(lines[0].d),
        }
    })
}

type Plane = {
    p0: Vector,
    normal: Vector,
}

function linePlaneIntersection(line: Line, plane: Plane): { p: Vector, t: Rational } {
    const a = dot(plane.p0.sub(line.p0), plane.normal)
    const b = dot(line.d, plane.normal)
    const s = a.div(b)
    const p = line.p0.add(line.d.mul(s))
    return { p, t: p.x.sub(line.p0.x).div(line.d.x) }
}

function findIntersectingLine(lines: Line[]): Vector {
    const [ _, h1, h2, h3 ] = normalizeToLine0(lines)

    const h11 = h1.p0
    const h12 = h1.p0.add(h1.d)
    const plane = { p0: h11, normal: cross(h11, h12) }
    const h21 = linePlaneIntersection(h2, plane)
    const h31 = linePlaneIntersection(h3, plane)

    const tDiff = h31.t.sub(h21.t)
    const dDiff = h31.p.sub(h21.p).div(tDiff)

    return h21.p.sub(dDiff.mul(h21.t)).add(lines[0].p0)
}

function part2(lines: Line[]): number {
    const r = findIntersectingLine(lines)
    return Math.round(r.x.asNumber()) + Math.round(r.y.asNumber()) + Math.round(r.z.asNumber())
}

export const Day24 = {
    number: 24,
    title: "Never Tell Me The Odds",
    parseInput,
    part1,
    part2,
}
