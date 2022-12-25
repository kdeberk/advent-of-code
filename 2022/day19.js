"use strict"
const shared = require('./shared.js')

const fs = require('fs')

const input = fs.readFileSync('./input/day19.txt', 'utf8')

const parseBlueprint = (line) => {
  const [idx, oreOre, clayOre, obsOre, obsClay, geodeOre, geodeObs] = line.matchAll(/\d+/g)
  return {idx: idx, ore: {ore: oreOre}, clay: {ore: clayOre}, obsidian: {ore: obsOre, clay: obsClay}, geode: {ore: geodeOre, obsidian: geodeObs}}
}

const blueprints = input.lines().map(parseBlueprint)

const max = Math.max
const solve = (bp, minLeft, rOre, nOre, rClay, nClay, rObsidian, nObsidian, rGeode, nGeode, seen) => {
  if(minLeft == 0) {
    return nGeode
  }

  const k = [minLeft, rOre, nOre, rClay, nClay, rObsidian, nObsidian, rGeode].join('-')
  if(seen[k] != undefined) {
    return seen[k]
  }

  let best = 0
  if(bp.geode.ore <= nOre && bp.geode.obsidian <= nObsidian) {
    // buy geode robot.
    best = max(best, solve(bp, minLeft-1, rOre, nOre+rOre-bp.geode.ore, rClay, nClay+rClay, rObsidian, nObsidian+rObsidian-bp.geode.obsidian, rGeode+1, nGeode+rGeode, seen))
  } else if(bp.obsidian.ore <= nOre && bp.obsidian.clay <= nClay) {
    // buy obsidian robot
    best = max(best, solve(bp, minLeft-1, rOre, nOre+rOre-bp.obsidian.ore, rClay, nClay+rClay-bp.obsidian.clay, rObsidian+1, nObsidian+rObsidian, rGeode, nGeode+rGeode, seen))
  } else {
    if(rOre < 4 && bp.ore.ore <= nOre) {
      // buy ore robot
      best = max(best, solve(bp, minLeft-1, rOre+1, nOre+rOre-bp.ore.ore, rClay, nClay+rClay, rObsidian, nObsidian+rObsidian, rGeode, nGeode+rGeode, seen))
    }
    if(bp.clay.ore <= nOre) {
      // buy clay robot
      best = max(best, solve(bp, minLeft-1, rOre, nOre+rOre-bp.clay.ore, rClay+1, nClay+rClay, rObsidian, nObsidian+rObsidian, rGeode, nGeode+rGeode, seen))
    }
    // don't buy anything
    best = max(best, solve(bp, minLeft-1, rOre, nOre+rOre, rClay, nClay+rClay, rObsidian, nObsidian+rObsidian, rGeode, nGeode+rGeode, seen))
  }
  seen[k] = best
  return best
}

console.log("Part1:", blueprints.map(bp => bp.idx*solve(bp, 24, 1, 0, 0, 0, 0, 0, 0, 0, {}, [])).sum())
console.log("Part2:", blueprints.slice(0,3).map(bp => solve(bp, 32, 1, 0, 0, 0, 0, 0, 0, 0, {}, [])).product())
