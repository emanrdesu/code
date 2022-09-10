#!/usr/bin/ruby

### yorn is a program for managing journals (yornals)
### uses git for version control
### TODO: finish first version of program
### TODO: add more documentation
### FUTURE: second version will implement encryption
### This is preliminary code

require 'optimist'

### Utility Functions

def alnum?(x)
  x =~ /\A\p{Alnum}+\z/
end

def yes_or_no?(q, pre=nil, post=nil)
  puts pre if pre
  loop do
    print (q + ' (yes/no): ')
    answer = gets.chomp.downcase
    if ["yes", "y", "no", "n"].any?(answer) then
      puts post if post
      return ["yes", "y"].any?(answer)
    else
      puts "Please enter 'yes', 'y', 'n', or 'no'."
    end
  end
end

def directoryDepth (dir)
  return 0 if not File.directory? dir
  Dir.children(dir).map {|i| 1 + directoryDepth([dir, i].join('/'))}.max or 1
end

def editor
  editors = ["zile", "ed", "nano", "vi", "emacs", "vim", "code"]
  ENV["EDITOR"] or editors.find {|b| File.exists? "/usr/bin/#{b}"} or "cat"
end

def mkdir(path)
  system "mkdir -p #{path} > /dev/null" and
    puts "Created #{path}." or
    exitError("could not create directory '%s'", path)
end

def exitError(message, *args)
  printf "Error, " + message + ".\n", *args
  exit 1
end

### Class additions

# Time methods addition
class Integer
  def second() self end
  def minute() self * 60.second end
  def hour() self * 60.minute end
  def day() self * 24.hour end
  def week() self * 7.day end
  def month() self * 4.week end
  def year() self * 12.month end
end

class Hash
  def flip
    x = {}
    self.each do |k,v|
      x[v] = k
    end
    x
  end
end


### Main Classes

class Yornal
  @@depth = {
    0 => :box,
    1 => :year,
    2 => :month,
    3 => :day,
    5 => :hour,
    6 => :minute,
  }

  attr_reader :name, :type

  def initialize(name)
    @name = name
    @type = @@depth[directoryDepth(path())]
  end

  def path
    $yornalPath + "/" + @name
  end

  def edit
    if @type == :box
      system "#{editor} #{path}"
      # git shit here
    else
      t = Time.now
      # only first 5 elements are relevant (m,h,d,mon,y)
      entryParent = path() + t.to_a[..5].reverse.take(@@depth.flip[@type] - 1).join("/")
      mkdir(entryParent)
      # git shit here
      f = [entryParent,
           (Dir.children(entryParent)
              .find {|e| t.send(@type).to_s == e} or t.send(@type).to_s)
          ].join("/")
      system "#{editor} #{f}"
      # git shit here
    end
  end
end

def main
  opts = Optimist::options do
  end

  xdg_data_home = (ENV["XDG_DATA_HOME"] and (File.expand_path ENV["XDG_DATA_HOME"]) + "/yornal")
  path = (ENV["YORNAL_PATH"] or xdg_data_home or "~/.yornal")
  $yornalPath = File.expand_path path

  if File.exist? $yornalPath
    if not File.directory? $yornalPath
      exitError("'%s' for yornal storage is not a directory", $yornalPath)
    end
  else
    mkdir yornalPath
  end
end
