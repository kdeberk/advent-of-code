type Card = {
    n: number
    winning: number[]
    got: number[]
}

function parseInput(input: string): Card[] {
    return input.split("\n").slice(0, -1).map((line) => {
        const [prefix, after] = line.split(": ")
        const [winning, got] = after.split(" | ")
        return {
            n: parseInt(prefix.split(" ")[1]),
            winning: winning.trim().split(/\s+/).map((n) => parseInt(n)),
            got: got.trim().split(/\s+/).map((n) => parseInt(n)),
        }
    })
}

function countMatches(card: Card): number {
    const winning = new Set(card.winning)
    return card.got.filter((n) => winning.has(n)).length
 }

function part1(cards: Card[]): number {
    let sum = 0;
    for(const card of cards) {
        const nMatches = countMatches(card)
        if(0 < nMatches) {
            sum += Math.round(Math.pow(2, nMatches - 1))
        }
    }
    return sum
}

function part2(cards: Card[]): number {
    const seen: Record<number, number> = {}

    function inner(cardIdx: number): number {
        if(cardIdx in seen) {
            return seen[cardIdx]
        }

        const nMatches = countMatches(cards[cardIdx])

        let nCards = 1;
        for(let idx = cardIdx + 1; idx < cardIdx + 1 + nMatches; idx++) {
            nCards += inner(idx)
        }
        seen[cardIdx] = nCards
        return nCards
    }

    let nCards = 0;
    for(let idx = 0; idx < cards.length; idx++) {
        nCards += inner(idx);
    }
    return nCards
}

export const Day4 = {
    number: 4,
    parseInput,
    part1,
    part2,
}
