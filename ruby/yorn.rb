#!/usr/bin/ruby

### yorn is a program for managing journals (yornals)
### uses git for version control
### TODO: add documentation
### TODO: implement querying yornal names
### FUTURE: support encryption

require 'optimist'
require 'openssl'

### globals

DEPTH = {
  box:   0,
  year:  1,
  month: 2,
  day:   3,
  hour:  4,
  min:   5
}

SHA256 = OpenSSL::Digest.new("SHA256")

### utility Functions

alias m method

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
    .delete_if {|i| i =~ /\D/} # remove nested yornals
    .map {|i| 1 + yornal_depth([dir, i].join('/'))}.max or 1
end

def tree (dir)
  return [dir] if !(File.directory? dir) || dir =~ /\.git/
  Dir.children(dir).map {|f| tree [dir,f].join('/')}.flatten
end

def editor
  editors = ["zile", "ed", "nano", "vi", "emacs", "vim", "code"]
  path = ENV["PATH"].split(':')
  ENV["EDITOR"] or editors.find {|b| path.any? {|p| File.exist? "#{p}/#{b}"}} or "cat"
end

def mkdir(path)
  system "mkdir -p #{path} > /dev/null" or
    exitError("could not create directory '%s'", path)
end

def exitError(message, *args)
  STDERR.printf "error: " + message + "\n", *args ; throw nil # exit 1
end

### stdlib class additions

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

class Time
  def box() '' end

  def path(x)
    # only first 5 elements are relevant (m,h,d,mon,y)
    self.to_a[..5].reverse.take([0, DEPTH[x] - 1].max).join('/')
  end
end

class String
  def lstrip_by(chars)
    self.gsub(Regexp.new("^[#{chars}]+"), '')
  end

  def rstrip_by(chars)
    self.gsub(Regexp.new("[#{chars}]+$"), '')
  end

  def strip_by(chars)
    self.lstrip_by(chars).rstrip_by(chars)
  end

  def stlip(c)
    self.strip_by(c).split(c)
  end

  def number?
    self =~ /^\d+$/
  end
end

class Array
  def jomp(x)
    self.join(x).chomp(x)
  end
end

### git functions

def git(*command) # appended
  system command.insert(0, "git").join(' ')
end

### main classes

class Yornal
  attr_reader :name, :type

  ## class methods

  def Yornal.create(name, type)
    {  /^\./   =>  "begin with a dot",
       /^\//   =>  "begin with /",
       /\/$/   =>  "end with /",
      /[\^]/   =>  "contain ^",
      /\/\//   =>  "contain consecutive /",
      /\.git/  =>  "contain '.git'",
      /^git$/  =>  "be 'git'",
      /^\d+$/  =>  "be digits only",
      /[^\/\._A-za-z0-9\-]/ =>  "have chars not in [a-z], [0-9] or [/_-.]"
    }.each do |regex, errorMessage|
      name =~ regex and exitError("name cannot " + errorMessage)
    end

    Yornal.list.find { |x| x == name } and exitError("yornal '#{name}' already exists")

    if type == :box
      File.open($yornalPath + '/' + name, "w") {}
      git(:add , $yornalPath + '/' + name)
      git(:commit, "-m \"create box yornal '#{name}'\"")
    else
      mkdir [$yornalPath, name, Time.now.path(type)].jomp('/')
      Yornal.new(name).edit("echo > ")
    end
  end

  def Yornal.delete(name, ask=true)
    Yornal.list.find { |x| x == name } or exitError("'#{name}' yornal doesn't exist")
    pre = "You are about to delete yornal '#{name}'."
    question = "Are you sure you want to delete it?"
    if (!ask || yes_or_no?(question, pre))
      Yornal.new(name).entries.each { |e| e.delete(ask=false) }
    end
  end

  def Yornal.list()
    tree($yornalPath)
      .delete_if { |v| v =~ /\.git/ }
      .map { |w| w[$yornalPath.size + 1 ..] }
      .map { |x| x.split('/') }
      .map { |y| y.take_while { |q| !q.number? } }
      .map { |z| z.jomp('/') }.uniq
  end

  ## instance methods

  def initialize(name)
    @name = name
    @type = DEPTH.flip[yornal_depth(path())]
  end

  def path
    $yornalPath + "/" + @name
  end

  def edit(editor = editor(), time=Time.now)
    entryParent = [path, time.path(@type)].jomp('/')
    mkdir(entryParent) unless @type == :box
    entry = [entryParent, time.send(@type).to_s].jomp('/')

    if File.exists? entry
      Entry.fromPath(entry).edit(editor)
    else
      system "touch #{entry}"
      Entry.fromPath(entry).edit(editor, action=:create)
      File.delete entry if File.size(entry) == 0
    end
  end

  def entries()
    self.query('*').map { |p| Entry.fromPath p }
  end

  # e.g. pattern: */*/8, 2022/09/* ; depends on yornal type (depth)
  def query(pattern)
    dateStructure = [:year, :month, :day, :hour, :min]
    datehash = -> x { x.zip(dateStructure).to_h.flip }

    tree(path()).filter do |path|
      entry = path[$yornalPath.size + @name.size + 1 ..]
      unless entry.split('/').join =~ /\D/ # remove nested yornals
        entryHash = datehash.(entry.stlip('/'))
        patternHash = datehash.(pattern.downcase.stlip('/'))

        patternHash.map do |k, v|
          (not entryHash[k]) or (v == '*') or
            v.split(',').map do |x|
            l, r = x.split('-')
            (l .. (r or l)) #ranges work for integer strings
          end.any? {|range| range.include? entryHash[k]}
        end.all? true
      end
    end
  end

  # first(:month, n = 2) ; first 2 months
  def first(x, n = 1, from = nil)
    return (from or entries)[.. (n-1)] if (x == :entry)

    t = (from ? from[0] : entries[0]).to_t
    k = t + n.send(x)
    (from or entries).filter {|e| e < k}
  end

  # last(:year, n = 3) ; last 3 years,
  def last(x, n = 1, from = nil)
    return (from or entries)[(-n) ..] if (x == :entry)

    t = from ? from[-1].to_t : Time.now
    k = t - n.send(x)
    (from or entries).filter {|e| e > k}
  end
end


class Entry
  include Comparable
  attr_reader :pdate, :yornal # pseudo date, yornal (obj or name)

  def <=>(x)
    to_t() <=> ((x.is_a? Entry) ? x.to_t : x)
  end

  def Entry.fromPath(path)
    path[$yornalPath.size ..].stlip('/')
      .partition { |x| x =~ /^\d+$/ }
      .map { |a| a.join('/') }
      .then { |date, yornal| Entry.new date, yornal }
  end

  def initialize(date, yornal)
    @date = (date.is_a? Time) ? date.to_a[..5].drop_while{|i| i == 0}.reverse.join('/') : date
    @yornal = (yornal.is_a? Yornal) ? yornal : Yornal.new(yornal)
  end

  def path
    [$yornalPath, name].jomp('/')
  end

  def name
    [@yornal.name, @date].jomp('/')
  end

  def to_t
    Time.new(*@date.split('/'))
  end

  def contains?(word)
    File.read(path).downcase =~ Regexp.new(word.downcase)
  end

  def matches?(regex)
    File.read(path) =~ Regexp.new(regex)
  end

  def edit(editor=editor(), action=:modify)
    digest = SHA256.digest(File.read(path()))
    system "#{editor} #{path}"
    unless digest == SHA256.digest(File.read(path()))
      git(:add, path())
      git(:commit, "-m '#{action} #{@yornal.name} entry #{@date}'")
    end
  end

  def delete(ask=true)
    pre = "You are about to delete yornal entry '#{name}'."
    question = "Are you sure you want to delete it?"
    git(:rm, "#{name}") if (!ask || yes_or_no?(question, pre))
  end

  def print(delimiter="\n\n")
    $stdout.print File.read(path)
    $stdout.print delimiter
  end

  def print_path(delimiter="\n")
    $stdout.print path
    $stdout.print delimiter
  end
end


### main

data_dir = (ENV["XDG_DATA_HOME"] and (File.expand_path ENV["XDG_DATA_HOME"]) + "/yornal")
path = (ENV["YORNAL_PATH"] or data_dir or "~/.yornal")
$yornalPath = File.expand_path path

if File.exist? $yornalPath
  if not File.directory? $yornalPath
    exitError("'%s' for yornal storage is not a directory", $yornalPath)
  end
else
  mkdir $yornalPath
  Dir.chdir $yornalPath
  git(:init)
end

Dir.chdir $yornalPath

opts = Optimist::options do
end
