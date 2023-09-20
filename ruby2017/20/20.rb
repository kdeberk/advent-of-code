#!/usr/bin/env ruby

Point = Struct.new(:x, :y, :z) do
  def +(other)
    Point.new(x + other.x, y + other.y, z + other.z)
  end

  def d(other)
    (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
  end
end

Particle = Struct.new(:position, :velocity, :acceleration) do
  def tick
    v = velocity + acceleration
    p = position + v
    Particle.new(p, v, acceleration)
  end

  def distance_to_zero
    position.d(Point.new(0, 0, 0))
  end
end

lines = File.read("input").split("\n")
particles = lines.map do |line|
  Particle.new(*line.scan(/<[-\d]+,[-\d]+,[-\d]+>/).map {|f| Point.new(*f.scan(/[-\d]+/).map(&:to_f))})
end

def part_1(particles)
  1_000.times do
    particles = particles.map(&:tick)
  end

  puts particles.index(particles.min_by(&:distance_to_zero))
end

def part_2(particles)
  1_000.times do
    particles = particles.map(&:tick).group_by(&:position).select {|k, v| 1 == v.length}.values.flatten
  end

  puts particles.length
end

part_1(particles.clone)
part_2(particles.clone)





