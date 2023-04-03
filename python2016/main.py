from datetime import datetime
import importlib
import os
import re
import sys

def importModules():
    days = {}
    for file in os.listdir(os.path.dirname(__file__)):
        if m := re.search(r'day(\d+).py', file):
            module = importlib.import_module(file[:-3])
            days[m[1]] = module
    return days

def timeSince(t):
    d = datetime.now()-t
    if d.seconds < 60:
        return f'{d.seconds}.{d.microseconds//1000:04}s'
    return f'{d.seconds//60:0}m.{d.seconds%60:0}s.{d.microseconds//1000}'

def runPart(part, fn, input):
    start = datetime.now()
    output = fn(input) if input else fn()
    print(f"  Part {part}: {str(output):12s} ({timeSince(start)})")

def runDay(day, module):
    print(module.NAME)
    if hasattr(module, 'SLOW'):
        print("  Part 1: **SKIPPED**")
        print("  Part 2: **SKIPPED**")
    elif hasattr(module, 'parseInput'):
        with open(f"input/day{day}.txt") as file:
            input = module.parseInput(file)

        runPart(1, module.part1, input)
        runPart(2, module.part2, input)
    else:
        runPart(1, module.part1, None)
        runPart(2, module.part2, None)

if __name__ == "__main__":
    days = importModules()
    if len(sys.argv) == 1:
        for k in sorted(days.keys(), key=int):
            runDay(k, days[k])
    else:
        k = sys.argv[1]
        runDay(k, days[k])
