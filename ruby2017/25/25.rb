#!/usr/bin/env ruby

Instruction = Struct.new(:new_value, :direction, :next_state)

class Parser
  # @return a hash that maps [state, current_value] to Instructions
  def parse_lines(lines)
    instructions = {}

    until lines.empty?
      line = lines.shift
      
      case line.strip
      when /^Begin in state ([A-Z]).$/
        @first_state = $1
      when /^Perform a diagnostic checksum after (\d+) steps.$/
        @n_steps = $1.to_i
      when /^In state ([A-Z]):$/
        @current_state = $1
      when /^If the current value is (\d):$/
        @current_value = $1.to_i
        instructions[[@current_state, @current_value]] =
            @current_instruction =
            Instruction.new(nil, nil, nil)
      when /^- Write the value (\d).$/
        @current_instruction.new_value = $1.to_i
      when /^- Move one slot to the right.$/
        @current_instruction.direction = :right
      when /^- Move one slot to the left.$/
        @current_instruction.direction = :left
      when /^- Continue with state ([A-Z]).$/
        @current_instruction.next_state = $1
      end
    end

    [@first_state, @n_steps, instructions]
  end

  def initialize
    @first_state = nil
    @n_steps = nil
    @current_state = nil    
    @current_value = nil
    @current_instruction = nil
  end
end

lines = File.read('input').split("\n")

position = 0
tape = Hash.new {0}
current_state, n_steps, instructions = Parser.new.parse_lines(lines)

n_steps.times do
  instruction = instructions[[current_state, tape[position]]]
  tape[position] = instruction.new_value
  case instruction.direction
  when :left then position -= 1
  when :right then position += 1
  end
  current_state = instruction.next_state
end

puts tape.values.count {|v| 1 == v}



