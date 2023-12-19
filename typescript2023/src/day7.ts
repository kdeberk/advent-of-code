import { add, countItems } from "./shared"

enum Type {
    FiveOfAKind = 0,
    FourOfAKind = 1,
    FullHouse = 2,
    ThreeOfAKind = 3,
    TwoPair = 4,
    OnePair = 5,
    High = 6,
}

type Hand = {
    cards: string[]
    bid: number
}

type TypedHand = Hand & { type: Type }

function parseInput(input: string): Hand[] {
    function parseHand(line: string): Hand {
        const [cards, bid] = line.split(" ")

        return { cards: cards.split(""), bid: parseInt(bid) }
    }

    return input.split("\n").map(parseHand)
}

function resolveType(hand: Hand): TypedHand {
    const counts: Record<string, number> = countItems(hand.cards)

    const maxCount = Math.max(...Object.values(counts))
    const nPairs = Object.values(counts).filter((x) => x == 2).length
    if (maxCount == 5) {
        return { ...hand, type: Type.FiveOfAKind }
    } else if (maxCount == 4) {
        return { ...hand, type: Type.FourOfAKind }
    } else if (maxCount == 3 && nPairs == 1) {
        return { ...hand, type: Type.FullHouse }
    } else if (maxCount == 3) {
        return { ...hand, type: Type.ThreeOfAKind }
    } else if (maxCount == 2 && nPairs == 2) {
        return { ...hand, type: Type.TwoPair }
    } else if (maxCount == 2) {
        return { ...hand, type: Type.OnePair }
    }
    return { ...hand, type: Type.High }
}

function applyJoker(hand: TypedHand): TypedHand {
    const jokerCount = hand.cards.filter((card) => card == "J").length

    if(jokerCount == 0) {
        return hand
    }
    switch(hand.type) {
        case Type.FiveOfAKind:
        case Type.FourOfAKind:
        case Type.FullHouse:
            return { ...hand, type: Type.FiveOfAKind }
        case Type.ThreeOfAKind:
            return { ...hand, type: Type.FourOfAKind }
        case Type.TwoPair: {
            if(jokerCount == 2) {
                return { ...hand, type: Type.FourOfAKind }
            }
            return { ...hand, type: Type.FullHouse }
        }
        case Type.OnePair:
            return { ...hand, type: Type.ThreeOfAKind }
        default:
            return { ...hand, type: Type.OnePair }
    }
}

function measureWinnings(hands: TypedHand[], order: string): number {
    const weights: Record<string, number> = {}
    for(let idx = 0; idx < order.length; idx++) {
        weights[order[idx]] = idx
    }

    function compareHands(a: TypedHand, b: TypedHand): -1 | 0 | 1 {
        if (a.type != b.type) {
            return a.type < b.type ? 1 : -1
        }
        for (let idx = 0; idx < 5; idx++) {
            const [wa, wb] = [weights[a.cards[idx]], weights[b.cards[idx]]]
            if(wa != wb) {
                return wa < wb ? 1 : -1
            }
        }
        return 0
    }

    return [...hands]
        .sort(compareHands)
        .map((hand, idx) => hand.bid * (idx + 1))
        .reduce(add)
}

function part1(hands: Hand[]): number {
    return measureWinnings(
        hands.map(resolveType),
        "AKQJT98765432"
    )
}

function part2(hands: Hand[]): number {
    return measureWinnings(
        hands.map(resolveType).map(applyJoker),
        "AKQT98765432J"
    );
}

export const Day7 = {
    number: 7,
    title: "Camel Cards",
    parseInput,
    part1,
    part2,
}
