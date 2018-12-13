#!/usr/bin/env ruby

def part_1(instructions)
  index = 0
  registers = 'abfip'.split('').map {|c| [c, 0]}.to_h
  last_played_sound = nil

  while index < instructions.length
    op, arg1, arg2 = instructions[index].split(' ')
    value1 = registers.key?(arg1) ? registers[arg1] : arg1.to_i
    value2 = registers.key?(arg2) ? registers[arg2] : arg2.to_i

    case op
    when 'set' then registers[arg1] = value2
    when 'add' then registers[arg1] += value2
    when 'mul' then registers[arg1] *= value2
    when 'mod' then registers[arg1] = registers[arg1] % value2
    when 'snd' then
      last_played_sound = value1
    when 'rcv' then 
      break if value1 > 0
    when 'jgz'
      if value1 > 0
        index += value2
        next
      end
    end

    index += 1
  end
  puts last_played_sound
end

def part_2(instructions)
  queue_a = Queue.new
  queue_b = Queue.new

  def process_instructions(p, instructions, to_me, to_other)
    index = 0
    registers = 'abfip'.split('').map {|c| [c, 0]}.to_h
    registers['p'] = p

    send_count = 0
    while index < instructions.length
      op, arg1, arg2 = instructions[index].split(' ')
      value1 = registers.key?(arg1) ? registers[arg1] : arg1.to_i
      value2 = registers.key?(arg2) ? registers[arg2] : arg2.to_i
    
      case op
      when 'set' then registers[arg1] = value2
      when 'add' then registers[arg1] += value2
      when 'mul' then registers[arg1] *= value2
      when 'mod' then registers[arg1] = registers[arg1] % value2
      when 'snd' then
        send_count += 1
        to_other.push(value1)
      when 'rcv' then registers[arg1] = to_me.shift
      when 'jgz'
        if value1 > 0
          index += value2
          next
        end
      end
      
      index += 1
    end
  rescue Exception
    puts "#{Time.now.nsec} #{send_count}\n"
  end

  Thread.new {process_instructions(0, instructions, queue_a, queue_b)}
  process_instructions(1, instructions, queue_b, queue_a)
end

instructions = File.read('input').split("\n")

part_1(instructions)
part_2(instructions)
