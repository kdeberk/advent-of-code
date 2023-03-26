# 2016, Day 11.
# A building complex of 4 floors contains generators and microchips. All these components
# need to be brought up or down via an elevator but there are rules that dictate how many
# components can be moved at the same time, and which components can be left together on the same floor.
#
# Calculate with a BFS through the 4^11 (4 states each for 5 generator + 5 chips + our hero) or
# 4^15 search domain. We prune redundant states by treating each generator-chip pair as equivalent.
#
# Part 1: What is the fewest number of steps needed to bring 5 generators and chips up to the top floor.
# Part 2: What is the fewest number of steps needed to bring 7 generators and chips up to the top floor.

from itertools import combinations

def solve(puzzle):
    def done(state):
        for floor in state:
            if floor < 3:
                return False
        return True

    def safe(state, floor):
        nGens = 0
        freeChip = False
        for idx in range(1, len(state), 2):
            if state[idx] == floor and state[idx+1] != floor:
                freeChip = True
            if state[idx+1] == floor:
                nGens += 1
        # Chips will be fried if their generator is on another floor and there is another generator on this floor.
        return not (freeChip and 0 < nGens)

    def digest(state):
        ts = [state[0]] + sorted((state[idx], state[idx+1]) for idx in range(1, len(state), 2))
        return hash(str(ts))

    def nexts(state):
        ns = []

        # pushIfSafe only checks src and dst floors.
        def pushIfSafe(state, dst, src):
            if safe(state, dst) and safe(state, src):
                ns.append(state)

        # move moves the items at idxs one floor up or down.
        def move(state, d, *idxs):
            st = state[:]
            st[0] += d
            for idx in idxs:
                st[idx] += d
            return st

        hero = state[0]
        idxs = [idx for idx in range(1, len(state)) if hero == state[idx]]
        # Hero moves a single item up or down.
        for idx in idxs:
            if 0 < hero:
                pushIfSafe(move(state, -1, idx), hero-1, hero)
            if hero < 3:
                pushIfSafe(move(state,  1, idx), hero+1, hero)

        # Hero moves two items up or down.
        for c in combinations(idxs, 2):
            a, b = c
            if 0 < hero:
                pushIfSafe(move(state, -1, a, b), hero-1, hero)
            if hero < 3:
                pushIfSafe(move(state,  1, a, b), hero+1, hero)
        return ns

    seen = set([])

    q = [(0, puzzle)]
    while 0 < len(q):
        (steps, state) = q.pop(0)

        if done(state):
            return steps

        steps += 1
        for nxt in nexts(state):
            d = digest(nxt)
            if (d not in seen):
                q.append((steps, nxt))
                seen.add(d)

def part1():
    # Order is hero, then chips and generators for Promethium, Plutonium, Ruthenium, Strontium and Thullium.
    return solve([0, 2, 2, 1, 0, 2, 2, 1, 0, 0, 0])

def part2():
    # Order is hero, then chips and generators for Dilithium, Elerium, Promethium, Plutonium, Ruthenium, Strontium and Thullium.
    return solve([0, 0, 0, 0, 0, 2, 2, 1, 0, 2, 2, 1, 0, 0, 0])


if __name__ == "__main__":
    print("Part 1:", part1())
    print("Part 2:", part2())
