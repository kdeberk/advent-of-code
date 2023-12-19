
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

export function rotateMatrix<T>(x: T[][]): T[][] {
    const res: T[][] = [...Array(x[0].length)].map(_ => Array(x.length));
    for(let idx = 0; idx < x.length; idx++) {
        for(let jdx = 0; jdx < x[0].length; jdx++) {
            res[jdx][idx] = x[idx][jdx];
        }
    }
    return res
}

export type LinkedItem<T> = {
    prv: LinkedItem<T> | null
    nxt: LinkedItem<T> | null
    item: T
}

export class LinkedList<T> {
    head: LinkedItem<T> | null = null
    tail: LinkedItem<T> | null = null

    add(x: T) {
        if(this.head === null) {
            this.head = { prv: null, nxt: null, item: x }
            this.tail = this.head
            return
        }
        const item = { prv: this.tail, nxt: null, item: x }
        this.tail!.nxt = item
        this.tail = item;
    }

    find(fn: (x: T) => boolean): LinkedItem<T> | null {
        let cur: LinkedItem<T> | null = this.head
        for (; cur != null; cur = cur!.nxt) {
            if (!fn(cur.item)) {
                continue
            }
            return cur
        }
        return null
    }

    remove(fn: (x: T) => boolean) {
        let cur: LinkedItem<T> | null = this.head
        for (; cur != null; cur = cur.nxt) {
            if (!fn(cur.item)) {
                continue
            }

            if (cur.prv !== null) {
                cur.prv.nxt = cur.nxt
            } else {
                this.head = cur.nxt
            }
            if (cur.nxt !== null) {
                cur.nxt.prv = cur.prv
            } else {
                this.tail = cur.prv
            }
            cur.prv = null
            cur.nxt = null
        }
    }
}

export class MinHeap<T> {
    private items: T[];

    constructor(private fn: (x: T) => number) {
        this.items = []
    }

    peek(): T | null {
        if(0 < this.items.length) {
            return this.items[0]
        }
        return null
    }

    push(t: T) {
        const score = this.fn(t);

        this.items.push(t)
        let idx = this.items.length - 1

        while(0 < idx) {
            const pdx = Math.floor((idx-1) / 2)
            if(this.fn(this.items[pdx]) < score) {
                return
            }
            this.swap(idx, pdx)
            idx = pdx
        }
    }

    pop(): T | null {
        if(this.items.length === 0) {
            return null
        } else if(this.items.length === 1) {
            return this.items.pop()!
        }

        this.swap(0, this.items.length - 1)
        const res = this.items.pop()!;

        let idx = 0
        const score = this.fn(this.items[0]);
        while(true) {
            const adx = 2 * idx + 1;
            const bdx = 2 * idx + 2;
            let largest = idx


            if(adx < this.items.length && this.fn(this.items[adx]) < this.fn(this.items[largest])) {
                largest = adx
            }
            if(bdx < this.items.length && this.fn(this.items[bdx]) < this.fn(this.items[largest])) {
                largest = bdx
            }

            if(largest === idx) {
                break
            }
            this.swap(idx, largest)
            idx = largest
            continue
        }

        return res;
    }

    private swap(idx: number, jdx: number) {
        const t = this.items[idx]
        this.items[idx] = this.items[jdx]
        this.items[jdx] = t
    }
}
