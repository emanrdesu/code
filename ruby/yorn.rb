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

def yornal_depth (dir)
  return 0 if not File.directory? dir
  Dir.children(dir)
    .filter {|i| not (i.downcase =~ /[a-z]/)} # remove nested yornals
    .map {|i| 1 + yornal_depth([dir, i].join('/'))}.max or 1
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

class String
  def strip_by(words)
    f = -> c { words.include? c }
    self.chars.drop_while(&f).reverse.drop_while(&f).reverse.join
  end

  def stlip(c)
    self.strip_by(c).split(c)
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
    @type = @@depth.flip[yornal_depth(path())]
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

  def entries()
    self.filter('*').map { |e| Entry.fromPath e }
  end

  # e.g. pattern: */*/8, 2022/09/* ; depends on yornal type (depth)
  def filter(pattern)
    dateStructure = [:year, :month, :day, :hour, :min]
    f = -> s { s.zip(dateStructure).to_h.flip }

    tree(path()).filter do |e|
      qux = e[$yornalPath.size + @name.size + 1 ..]
      unless qux.downcase =~ /[a-z]/ # remove nested yornals
        foo = f.(qux.stlip('/'))
        bar = f.(pattern.downcase.stlip('/'))

        bar.map do |k,v|
          (not foo[k]) or (v == '*') or
            v.split(',').map do |x|
            l, r = x.split('-')
            (l .. (r or l))
          end.any? {|r| r.include? foo[k]}
        end.all? true
      end
    end
  end

  # last(:year, n = 3) ; last 3 years,
  def last(x, n = 1, from = nil)
    return (from or entries())[(-n) ..] if (x == :entry)

    t = from ? from[-1].to_t : Time.now
    k = t - n.send(x)
    (from or entries()).filter {|e| e > k}
  end
end


class Entry
  include Comparable
  attr_reader :pdate, :yornal # pseudo date, yornal (obj or name)

  def <=>(x)
    (to_t() <=> ((x.is_a? Entry) ? x.to_t : x))
  end

  def Entry.fromPath(p)
    yornal, *pdate = p[$yornalPath.size ..].stlip('/')
    Entry.new(pdate.join('/'), yornal)
  end

  def initialize(date, yornal)
    @yornal = (yornal.is_a? Yornal) ? yornal : Yornal.new(yornal)
    @pdate = (date.is_a? Time) ? date.to_a[..5].drop_while{|i| i == 0}.reverse.join('/') : date
  end

  def path
    @yornal.path() + "/" + @pdate
  end

  def to_t
    Time.new(*@pdate.split('/'))
  end
end


### main

opts = Optimist::options do
end

data_dir = (ENV["XDG_DATA_HOME"] and (File.expand_path ENV["XDG_DATA_HOME"]) + "/yornal")
path = (ENV["YORNAL_PATH"] or data_dir or "~/.yornal")
$yornalPath = File.expand_path path

if File.exist? $yornalPath
  if not File.directory? $yornalPath
    exitError("'%s' for yornal storage is not a directory", $yornalPath)
  end
else
  mkdir $yornalPath
end
