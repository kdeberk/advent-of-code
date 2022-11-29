#!/usr/bin/env ruby

require 'erb'

class AssemblyFile
  attr_reader :name, :object, :target

  def initialize(name)
    @name = name
    @basename = File.basename(name, ".asm")
    @object = "#{@basename}.o"
    @target = @basename
  end

  def main_file?
    @name.start_with?("day")
  end

  def includes
    File.readlines(@name)
      .select {|line| line.start_with?("%include")}
      .map {|line| (/^%include "(.+)"/.match(line))[1]}
      .sort
  end

  def input_file
    "data/#{@basename}.txt" if File.exist?("data/#{@basename}.txt")
  end
end

files = Dir
          .glob("*.asm")
          .map(&AssemblyFile.method(:new))
          .sort_by(&:name)

template = %q{
LD_FLAGS=-m elf_i386
NASM_FLAGS=-f elf64 -F dwarf -g

.PHONY: all
all: run

.PHONY: clean
clean:
	rm *.o <%= files.select(&:main_file?).map(&:target).join(' ') %>

.PHONY: run
run: <%= files.select(&:main_file?).map(&:target).join(' ') %>
% files.select(&:main_file?).each do |file|
	./<%= file.target %>
% end

% files.select(&:main_file?).each do |file|
<%= file.target %>: <%= file.object %> <%= file.input_file %>
	ld $(LD_FLAGS) -o <%= file.target %> <%= file.object %>
% end

% files.select(&:main_file?).each do |file|
<%= file.object %>: <%= file.name %> <%= file.includes.join(' ') %>
	nasm $(NASM_FLAGS) <%= file.name %>
% end

% files.reject(&:main_file?).each do |file|
<%= file.name %>: <%= file.includes.join(' ') %>
% end
}

generator = ERB.new(template, trim_mode: "%")
File.write("Makefile", generator.result)
