#!/usr/bin/env ruby
#
# itunes
#
# AUTHOR: Chris Barrett <chris.d.barrett@me.com>
# LICENSE: BSD
#
# Copyright (c) 2013, Chris Barrett
#
#
# DESCRIPTION:
#
#  Commands for working with iTunes from the command-line.
#

require 'fileutils'
require 'io/console'
require 'pathname'
require 'colorize'

def print_usage
  puts <<-EOF
itunes: Commands for working with iTunes from the command-line.

Usage:
  add [items...]    Add files or folders to the iTunes library
  help              Show usage

EOF
end

# Find the iTunes Media folder. If it is not set in the environment,
# use the default location.
ITUNES_MEDIA = ENV['ITUNES_MEDIA'] || File.join(Dir.home,
                                                'Music',
                                                'iTunes',
                                                'iTunes Media')

# Prompt the user for yes-or-no input with a default value.
def read_y_or_n(default)
  case STDIN.getch.downcase
  when 'y'  then true
  when 'n'  then false
  when '\r', '\n' then default
  else
    puts 'Enter `y` or `n`.'.red
    read_y_or_n default
  end
end

class String
  def pluralize(n)
    n == 1 ? self : "#{self}s"
  end
end

# Traverse the directory tree, returning all the files in the hierarchy.
def files_in_tree(d)
  Dir.chdir(d) do
    Dir['**'].map do |f|
      f = File.join(d, f)
      File.directory?(f) ? files_in_tree(f) : f
    end
  end.flatten
end

# Add a collection of items to the iTunes library. Accepts both files and
# directories.
def add_to_itunes(xs)
  # Get all the files in the collection and sort by name.
  files = xs
    .map { |x| File.absolute_path x }
    .map { |x| File.directory?(x) ? files_in_tree(x) : x }
    .flatten
  # Copy files, showing progress.
  dest = File.join(ITUNES_MEDIA, 'Automatically Add to iTunes.localized')
  files.each do |f|
    FileUtils.cp(f, dest)
    puts " #{'+'.green} #{Pathname.new(f).basename}"
  end
  # Delete the original items if requested by the user.
  n = files.count
  puts "Delete original #{'item'.pluralize n}? (y/n) [n] "
  if read_y_or_n(false)
    n = xs.each do |x|
      FileUtils.rm_r x
      puts " #{'D'.red} #{x}"
    end.count
    puts "Deleted #{n} #{'item'.pluralize n}."
  end
end

# Run main.
if __FILE__ == $PROGRAM_NAME
  cmd, *xs = ARGV
  case cmd
  when 'help' || '-h' || '--help' then print_usage
  when 'add' then
    abort 'Cannot find iTunes Media folder' unless Dir.exists? ITUNES_MEDIA
    targets, noexist = xs.partition { |x| File.exists? x }
    if noexist.any?
      puts 'Warning: the following items do not exist:'.yellow
      noexist.each { |x| puts " #{'?'.yellow} #{x}" }
      puts
    end
    if targets.any?
      add_to_itunes targets
    else
      abort 'No items added.'.red
    end
  else
    if cmd then abort "Unknown command: #{cmd}".red
    else
      print_usage
    end
  end
end
