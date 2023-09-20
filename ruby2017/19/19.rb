#!/usr/bin/env ruby

maze = File.read('input').split("\n")

State = Struct.new(:x, :y, :direction)

def char_at(maze, state)
  maze[state.y][state.x]
end

def move(state)
  case state.direction
  when :up
    State.new(state.x, state.y - 1, state.direction)
  when :down
    State.new(state.x, state.y + 1, state.direction)
  when :left
    State.new(state.x - 1, state.y, state.direction)
  when :right
    State.new(state.x + 1, state.y, state.direction)
  end
end

def change_direction(maze, state)
  case state.direction
  when :up, :down
    [State.new(state.x, state.y, :left), State.new(state.x, state.y, :right)]
  when :left, :right
    [State.new(state.x, state.y, :up), State.new(state.x, state.y, :down)]
  end.find {|new_state| ' ' != char_at(maze, move(new_state))}
end

state = State.new(maze.first.index('|'), -1, :down)
letters = ''
n_steps = 0

loop do
  state = move(state)
  char = char_at(maze, state)

  case char
  when ' '      then break
  when /[A-Z]/  then letters += char
  when '-', '|'
  when '+'      then state = change_direction(maze, state)
  end
  n_steps += 1
end

puts letters
puts n_steps
