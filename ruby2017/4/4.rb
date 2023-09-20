#!/usr/bin/env ruby

lines = File.read('input').split("\n")

uniq_count = 0
anagram_count = 0
lines.select do |line|
  words = line.split(" ")
  anagrams = words.map {|w| w.split('').sort}.uniq

  uniq_count += 1 if words.length == words.uniq.length
  anagram_count += 1 if words.length == anagrams.uniq.length
end

puts uniq_count
puts anagram_count
