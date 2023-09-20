#!/usr/bin/env ruby

steps = File.read('input').strip.split(',')

Cell = Struct.new(:x, :y, :z) do
  def self.origin
    new(0, 0, 0)
  end

  def self.northeast_of(cell)
    new(cell.x + 1, cell.y - 1, cell.z)
  end

  def self.north_of(cell)
    new(cell.x + 1, cell.y, cell.z - 1)
  end

  def self.northwest_of(cell)
    new(cell.x, cell.y + 1, cell.z - 1)
  end

  def self.southwest_of(cell)
    new(cell.x - 1, cell.y + 1, cell.z)
  end

  def self.south_of(cell)
    new(cell.x - 1, cell.y, cell.z + 1)
  end

  def self.southeast_of(cell)
    new(cell.x, cell.y - 1, cell.z + 1)
  end

  def distance_to(other)
    ((x - other.x).abs + (y - other.y).abs + (z - other.z).abs) / 2
  end
end

current = Cell.origin
max_distance = 0

steps.each do |step|
  current =
      case step
      when 'ne' then Cell.northeast_of(current)
      when 'n'  then Cell.north_of(current)
      when 'nw' then Cell.northwest_of(current)
      when 'sw' then Cell.southwest_of(current)
      when 's'  then Cell.south_of(current)
      when 'se' then Cell.southeast_of(current)
      else
        raise step
      end
  max_distance = [max_distance, current.distance_to(Cell.origin)].max
end

puts current.distance_to(Cell.origin)
puts max_distance


