#!/usr/bin/ruby

### yorn is a program for managing journals (yornals)
### uses git for version control
### TODO: add documentation
### FUTURE: second version will implement encryption

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
    .filter {|i| not (i =~ /\D/)} # remove nested yornals
    .map {|i| 1 + yornal_depth([dir, i].join('/'))}.max or 1
end

def tree (dir)
  return [dir] if not (File.directory? dir)
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
  STDERR.printf "error: " + message + ".\n", *args ; throw nil # exit 1
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
    (name =~ /[\/\.\-_A-za-z]/) or exitError("name must contain [a-z] or [/_-.]")
    (name =~ /[^\/\._A-za-z0-9\-]/) and exitError("name can only have [a-z], [0-9] or [/_-.]")
    (name =~ /\/\//) and exitError("name cannot contain consecutive /")
    (name =~ /^\//) and exitError("name cannot begin with /")
    (name =~ /\/$/) and exitError("name cannot end with /")
    (name =~ /\.git/) and exitError("name cannot contain '.git'")
    (name =~ /^\.+$/) and exitError("name cannot be dots only")
    Yornal.list.find { |x| x == name } and exitError("#{name} yornal already exists")

    if type == :box
      File.open($yornalPath + '/' + name, "w") {}
      git(:add , $yornalPath + '/' + name)
      git(:commit, "-m 'create box yornal #{name}'")
    else
      mkdir [$yornalPath, name, Time.now.path(type)].join('/')
      Yornal.new(name).edit("touch")
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
      .filter { |x| !(x =~ /\.git/)}
      .map { |x| x.rstrip_by "/0-9" }.uniq
      .map { |x| x[$yornalPath.size + 1 ..] }
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
    entryParent = [path, time.path(@type)].join('/').chomp('/')
    mkdir(entryParent) unless @type == :box
    entry = [entryParent, time.send(@type).to_s].join('/').chomp('/')
    digest = (File.exists? entry) && SHA256.digest(File.read entry)
    system "#{editor} #{entry}"

    # git stuff
    if File.exists?(entry)
      unless digest == SHA256.digest(File.read entry)
        git(:add, entry)
        sentry = entry[$yornalPath.size + 1 + @name.size ..].lstrip_by('/') # short entry
        git(:commit, "-m '#{digest ? "modify" : "create"} #{@name} entry #{sentry}'")
      end
    end
  end

  def entries()
    self.query('*').map { |p| Entry.fromPath p }
  end

  # e.g. pattern: */*/8, 2022/09/* ; depends on yornal type (depth)
  def query(pattern)
    dateStructure = [:year, :month, :day, :hour, :min]
    f = -> s { s.zip(dateStructure).to_h.flip }

    tree(path()).filter do |e|
      qux = e[$yornalPath.size + @name.size + 1 ..]
      unless qux.split('/').join =~ /\D/ # remove nested yornals
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
    (to_t() <=> ((x.is_a? Entry) ? x.to_t : x))
  end

  # TODO: FIX
  def Entry.fromPath(p)
    yornal, *pdate = p[$yornalPath.size ..].stlip('/')
    Entry.new(pdate.join('/'), yornal)
  end

  def initialize(date, yornal)
    @yornal = (yornal.is_a? Yornal) ? yornal : Yornal.new(yornal)
    @pdate = (date.is_a? Time) ? date.to_a[..5].drop_while{|i| i == 0}.reverse.join('/') : date
  end

  def path
    (@yornal.path() + "/" + @pdate).chomp('/')
  end

  def name
    path[$yornalPath.size + 1 ..]
  end

  def to_t
    Time.new(*@pdate.split('/'))
  end

  def edit
    digest = SHA256.digest(File.read(path()))
    system "#{editor} #{path}"
    unless digest == SHA256.digest(File.read(path()))
      git(:add, path())
      git(:commit, "-m 'modify #{@yornal.name} entry #{@pdate}'")
    end
  end

  def delete(ask=true)
    pre = "You are about to delete yornal entry '#{name}'."
    question = "Are you sure you want to delete it?"
    git(:rm, "#{name}") if (!ask || yes_or_no?(question, pre))
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
