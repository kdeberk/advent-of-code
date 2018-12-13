#!/usr/bin/env ruby

def part_1(instructions)
  registers = 'abcdefgh'.split('').map {|k| [k, 0]}.to_h

  index = 0
  mul_count = 0
  while instructions.length > index
    instruction = instructions[index]
    operator, arg1, arg2 = instruction.split(' ')

    value1 = registers.key?(arg1) ? registers[arg1] : arg1.to_i
    value2 = registers.key?(arg2) ? registers[arg2] : arg2.to_i

    case operator
    when 'set'
      puts ['b', 'd', 'e', 'h'].map {|k| [k, registers[k]]}.map {|k, v| "#{k}: #{v}"}.join('; ') if arg1 == 'f' && value2 == 0
      registers[arg1] = value2
    when 'sub'
      registers[arg1] -= value2
    when 'mul'
      registers[arg1] *= value2
      mul_count += 1
    when 'jnz'
      if 0 != value1
        index += value2
        next
      end
    end

    index += 1
  end

  puts mul_count
end


instructions = File.read('input').split("\n")
part_1(instructions)
part_1(["set a 1"] + instructions)
