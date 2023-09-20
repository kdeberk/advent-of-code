#!/usr/bin/env ruby

require 'byebug'

def loadModules
  Dir["day*.rb"].map do |filename|
    require_relative filename

    eval(File.basename(filename, ".rb").capitalize)
  end.sort_by(&:name)
end

def timeSince(t)
  d = Time.now - t
  if d < 60
    return "%02ds.%03d" % [d, (d % 1) * 1000]
  end
  "%dm.%02ds.%03d" % [d / 60, d % 60, (d % 1) * 1000]
end

def runPart(part, fn, input)
  start = Time.now
  output = input ? fn.call(input) : fn.call()
  puts "  Part %s: %-12s (%s)" % [part, output, timeSince(start)]
end

def runDay(day, mod, inputFile)
  puts(mod.NAME)

  input = nil
  if mod.respond_to?(:parse_input)
    inputFile = "input/day#{day}.txt" if inputFile == nil
    start = Time.now
    input = mod.parse_input(File.read(inputFile))
    puts  "  parse_input:         (%s)" % timeSince(start)
  end

  runPart(1, mod.method(:part1), input) if mod.respond_to?(:part1)
  runPart(2, mod.method(:part2), input) if mod.respond_to?(:part2)
end

# TODO: handle days and input argument

loadModules.each_with_index do |mod, idx|
  runDay(idx+1, mod, nil)
end
