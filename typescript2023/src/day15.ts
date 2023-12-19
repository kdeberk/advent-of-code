import { LinkedList, add } from "./shared";

type AddOrReplaceLensStep = {
    step: string
    label: string
    operation: "="
    focalLength: number
}

type RemoveLensStep = {
    step: string
    label: string
    operation: "-"
}

type Step = AddOrReplaceLensStep | RemoveLensStep

type Sequence = Step[]

function HASH(str: string) {
    let value = 0;
    for(let idx = 0; idx < str.length; idx++) {
        value = ((value + str.charCodeAt(idx)) * 17) % 256
    }
    return value
}

function parseInput(input: string): Sequence {
    function parseStep(step: string): Step {
        if(step.endsWith("-")) {
            return { step, label: step.slice(0, step.length - 1), operation: "-" }
        }

        const idx = step.indexOf("=")
        return { step, label: step.slice(0, idx), operation: "=", focalLength: parseInt(step.slice(idx+1)) }
    }
    
    return input.split(",").map(parseStep)
}

function part1(seq: Sequence): number {
    return seq.map((step) => HASH(step.step)).reduce(add)
}

type Lens = {
    label: string
    focalLength: number
}

type Box = {
    num: number,
    lenses: LinkedList<Lens>,
    set: Set<string>,
}

function applyStep(step: Step, box: Box): void {
    const labelFn = (lens: Lens) => lens.label == step.label

    if(step.operation === "-") {
        if(!box.set.has(step.label)) {
            return
        }
        box.set.delete(step.label)
        box.lenses.remove(labelFn)
        return
    }

    const item = box.lenses.find(labelFn)
    if(item !== null) {
        item.item.focalLength = step.focalLength
        return
    }
    box.lenses.add({ label: step.label, focalLength: step.focalLength })
    box.set.add(step.label)
}

function part2(seq: Sequence): number {
    const boxes: Box[] = [...Array(256)].map((_, n) => { return { num: n, lenses: new LinkedList(), set: new Set() }})
    for(const step of seq) {
        applyStep(step, boxes[HASH(step.label)])
    }

    let score = 0;
    for(const box of boxes) {
        for(let item = box.lenses.head, idx = 0; item !== null; item = item.nxt, idx++) {
            score += (box.num + 1) * (idx + 1) * item.item.focalLength
        }
    }

    return score
}

export const Day15 = {
    number: 15,
    title: "Lens Library",
    parseInput,
    part1,
    part2,
}
