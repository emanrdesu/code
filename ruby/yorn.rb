#!/usr/bin/ruby

### yorn is a program for managing journals (yornals)
### uses git for version control
### TODO: finish first version of program
### TODO: add documentation
### FUTURE: second version will implement encryption
### This is early preliminary code, much to be done

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

def tree (dir)
  (File.directory? dir) ? Dir.children(dir).map {|f| tree [dir,f].join('/')}.flatten : [dir]
end

def editor
  editors = ["zile", "ed", "nano", "vi", "emacs", "vim", "code"]
  ENV["EDITOR"] or editors.find {|b| File.exists? "/usr/bin/#{b}"} or "cat"
end

def mkdir(path)
  if system "mkdir -p #{path} > /dev/null"
    puts "Created #{path}."
  else
    exitError("could not create directory '%s'", path)
  end
end

def exitError(message, *args)
  printf "Error, " + message + ".\n", *args
  exit 1
end



### Stdlib Class additions

class Hash
  def flip
    x = {}
    self.each do |k,v|
      x[v] = k
    end
    x
  end
end

class String
  def strip_by(words)
    f = -> c { words.include? c }
    self.chars.drop_while(&f).reverse.drop_while(&f).reverse.join
  end
end


### Main Classes

class Yornal
  @@depth = {
    box:   0,
    year:  1,
    month: 2,
    day:   3,
    hour:  4,
    min:   5
  }

  @@months = {
    january: 1,
    february: 2,
    march: 3,
    april: 4,
    may: 5,
    june: 6,
    july: 7,
    august: 8,
    september: 9,
    october: 10,
    november: 11,
    december: 12
  }

  attr_reader :name, :type

  def Yornal.create(name, type)
  end

  def initialize(name)
    @name = name
    @type = @@depth.flip[directoryDepth(path())]
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
      entryParent = path() + t.to_a[..5].reverse.take(@@depth[@type] - 1).join("/")
      mkdir(entryParent)
      # git shit here
      f = [entryParent,
           (Dir.children(entryParent)
              .find {|e| t.send(@type).to_s == e} or t.send(@type).to_s)
          ].join('/')
      system "#{editor} #{f}"
      # git shit here
    end
  end

  # e.g. pattern: */*/8, 2022/aug/*, */*/*/*/* ; depends on yornal type (depth)
  def filter(pattern)
    dateStructure = [:year, :month, :day, :hour, :min]
    f = -> s { s.zip(dateStructure).to_h.flip }

    tree(path()).filter do |e|
      foo = f.(e[$yornalPath.size + @name.size + 1 ..].split('/').filter {|x| not x.empty? })
      bar = f.(pattern.downcase.strip_by('/').split('/'))

      bar.map do |k,v|
        if k == :month
          (not foo[k]) or
            ((@@months.filter {|j| j =~ Regexp.new(v)}.values.map(&:to_s) + [v]).any? (foo[k]))
        else
          (not foo[k]) or (v == '*') or (v == foo[k])
        end
      end
        .all? true
    end
  end

  def last(x)
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
    mkdir $yornalPath
  end
end

main()
