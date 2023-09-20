#!/usr/bin/env ruby

lines = File.read('input').split("\n")

registers = {}

def condition_holds?(condition, registers)
  register, operation, constant = condition.match(/^([a-z]+) ([^\s]+) ([\-0-9]+)/)[1..3]

  registers[register] = 0 if !registers.key?(register)
  constant = constant.to_i

  case operation
  when '>'  then registers[register] > constant
  when '>=' then registers[register] >= constant
  when '<'  then registers[register] < constant
  when '<=' then registers[register] <= constant
  when '!=' then registers[register] != constant
  when '==' then registers[register] == constant
  else
    raise "Unknown op #{operation}"
  end
end

def apply_instruction(instruction, registers)
  register, operation, constant = instruction.match(/^([a-z]+) ([a-z]+) ([\-0-9]+)/)[1..3]

  registers[register] = 0 if !registers.key?(register)
  constant = constant.to_i 

  case operation
  when 'inc' then registers[register] += constant
  when 'dec' then registers[register] -= constant
  else
    raise "Unknown op #{operation}"
  end
end

highest_seen = 0
lines.each do |line|
  instruction, condition = line.match(/^(.*) if (.*)$/)[1..2]

  if condition_holds?(condition, registers)
    apply_instruction(instruction, registers)   
    
    highest_seen = [highest_seen, registers.values.max].max
  end    
end

puts registers.values.max
puts highest_seen
