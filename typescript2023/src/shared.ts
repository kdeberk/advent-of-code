
export function uniq<T>(ts: T[]): T[] {
    return Array.from(new Set(ts))
}

export function add(a: number, b: number): number {
    return a + b
}

export function mult(a: number, b: number): number {
    return a * b
}

export function countItems<T extends keyof any>(coll: T[]): Record<T, number> {
    const counts = {} as Record<T, number>
    for (const item of coll) {
        if (item in counts) {
            counts[item]! += 1
        } else {
            counts[item] = 1
        }
    }
    return counts
}

export function gcd(a: number, b: number): number {
    if(b < a) {
        return gcd(b, a)
    }
    return b % a == 0 ? a : gcd(b, b % a)
}

export function lcm(a: number, b: number): number {
    return a*b / gcd(a, b)
}

export function intersect<T>(a: Set<T>, b: Set<T>): Set<T> {
    const intersect = new Set<T>()
    for(const x of a) {
        if(b.has(x)) {
            intersect.add(x)
        }
    }
    return intersect
}

export function union<T>(a: Set<T>, b: Set<T>): Set<T> {
    return new Set([...a, ...b])
}
