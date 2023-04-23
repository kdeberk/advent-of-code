#!/usr/bin/env python
# main.py is for executing every day in sequence.
#  Usage:
#
# Execute every day in sequence:
#   ./main.py --all
# Execute a single day:
#   ./main.py --day <n>
# Execute a single day with the specified input file:
#   ./main.py --day <n> --input <file>

from datetime import datetime
import argparse
import importlib
import os
import re

parser = argparse.ArgumentParser()
parser.add_argument('--day')
parser.add_argument('--input')
parser.add_argument('--all', action="store_true")
args = vars(parser.parse_args())

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
    return f'{d.seconds//60:0}m.{d.seconds%60:0}s.{d.microseconds//1000:04}'

def runPart(part, fn, input):
    start = datetime.now()
    output = fn(input) if input else fn()
    print(f"  Part {part}: {str(output):12s} ({timeSince(start)})")

def runDay(day, module, inputFile=None):
    print(module.NAME)

    input = None
    if hasattr(module, 'parseInput'):
        if inputFile == None:
            inputFile = f"input/day{day}.txt"
        start = datetime.now()
        with open(inputFile) as file:
            input = module.parseInput(file)
        print(f"  parseInput:          ({timeSince(start)})")

    if hasattr(module, 'part1'):
        runPart(1, module.part1, input)
    if hasattr(module, 'part2'):
        runPart(2, module.part2, input)

def skipSlowDay(module):
    print(module.NAME)
    print("  Part 1: **SKIPPED**")
    print("  Part 2: **SKIPPED**")

if __name__ == "__main__":
    days = importModules()
    if k := args['day']:
        runDay(k, days[k], args['input'])
    else:
        for k in sorted(days.keys(), key=int):
            module = days[k]
            if hasattr(module, 'SLOW') and not args['all']:
                skipSlowDay(module)
            else:
                runDay(k, module)
