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
  STDERR.printf "Error: " + message + "\n", *args ; throw nil # exit 1
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

  def to_ss()
    (self < 0 ? '' : '+') + self.to_s
  end
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
    self.query('@').map { |p| Entry.fromPath p }.sort
  end

  # e.g. pattern: @/@/8, 2022/09/@ ; depends on yornal type (depth)
  def query(pattern)
    dateStructure = [:year, :month, :day, :hour, :min]
    datehash = -> x { x.zip(dateStructure).to_h.flip }

    tree(path()).filter do |path|
      entry = path[$yornalPath.size + @name.size + 1 ..]
      unless entry.split('/').join =~ /\D/ # remove nested yornals
        entryHash = datehash.(entry.stlip('/'))
        patternHash = datehash.(pattern.downcase.stlip('/'))

        patternHash.map do |k, v|
          (not entryHash[k]) or (v == '@') or
            v.split(',').map do |x|
            l, r = x.split('-')
            (l .. (r or l)) #ranges work for integer strings
          end.any? {|range| range.include? entryHash[k]}
        end.all? true
      end
    end
  end

  def first(x, n = 1, from = entries)
    return from[.. (n-1)] if (x == :entry)
    t = from[0].to_t
    from.filter { |e| e < (t + n) }
  end

  def last(x, n = 1, from = entries)
    return from[(-n) ..] if (x == :entry)
    t = Time.now
    from.filter { |e| e > (t - n) }
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


## documentation and command line parsing

class Format
  def self.examples(examples)
    return [] if examples.size == 0
    examples
      .map { |e| (' ' *  2) + e }
      .unshift("examples:")
      .join("\n")
  end

  def self.syntax(syntax)
    [syntax].flatten.join("\n")
  end

  def self.monthswap(string)
    string = string.downcase

    [ "january",
      "february",
      "march",
      "april",
      "may",
      "june",
      "july",
      "august",
      "september",
      "october",
      "november",
      "december"
    ] .map { |m| [m[0..2], m].map {|x| Regexp.new(x)} }
      .zip(1..) do |names, i|
      names.each { |n| string.gsub!(n, i.to_s) }
    end

    string
  end
end


class Parse

  def self.editFlag(argument, entries) # => Time
    head = -> n=0 { entries[n] }
    tail = -> n=0 { entries[entries.size - 1 + n] }
    middle = -> n=0 { entries[(entries.size / 2) + n] }

    location, *operands = argument.split(/[\+\-]/)
    ops = argument.scan(/[\+\-]/)

    { ["t", "tail"] => tail,
      ["h", "head"] => head,
      ["m", "mid", "middle"] => middle
    } .find { |k,v| k.any? location }
      .tap {|_| _ or exitError "undefined location '#{location}'" }
      .then do |_, locator|
      return locator[].to_t if operands.size == 0

      arg = 0
      if operands[0].number?
        arg = (ops.shift + operands.shift).to_i
      end

      op = ops.shift
      anchor = locator[arg]
      anchor or exitError("entry $#{location}#{arg.to_ss} does not exist")

      operands.map {|x| self.timeLiteral x}
        .zip(ops + ['']).flatten.join
        .then { |time| anchor.to_t + (op == '+' ? 1 : -1) * (eval(time) || 0) }
    end
  end

  def self.lastFirstFlag(argument) # [Symbol, Integer]
    return [:entry, argument.to_i] if argument.number?
    operands = argument.split(/[\+\-]/)
    ops = argument.scan(/[\+\-]/)

    operands.map {|x| self.timeLiteral x}
      .zip(ops + ['']).flatten.join
      .then { |r| [:time, eval(r)] }
  end

  def self.timeLiteral(x) # Integer
    x =~ /\d+\.[a-z]+/ or exitError("malformed time spec '#{x}'")
    n, field = x.split('.')

    [ [:second, "s", "sec", "second"],
      [:minute, "min", "minute"],
      [:hour, "h", "hour"],
      [:day, "d", "day"],
      [:week, "w", "week"],
      [:month, "mon", "month"],
      [:year, "y", "year"]
    ].find do |m, *forms|
      forms.any? field
    end
      .tap {|_| _ or exitError "undefined time field '#{field}'"}
      .slice(0).then { |m| n.to_i.send(m) }
  end
end


$options = {
  last: {
    default: "1",

    syntax: [
      "[$n | timeSpan[±timeSpan]*]",
      "  where $j, $n ∈ NaturalNumber",
      "  and timeSpan ::= [$j.]dateAttr",
      "  and dateAttr ::= y[ear] | mon[th] | w[eek]",
      "                |  d[ay]  | h[our]  | min[ute]"
    ],

    examples: [
      "# selects the last entry in foo",
      "yorn foo --last",
      "# all the entries in foo in the past 3 years",
      "yorn foo --last 3.year",
      "# last 4 entries in bar yornal",
      "yorn bar --last 4",
      "# all the entries in qux in the past 2 month + 3 days",
      "yorn qux --last 3.day+2.mon",
      "# default action for multiple entries is to print entry paths",
      "# see -h for recommended usage"
    ]
  },

  first: {
    default: "1",

    syntax: [
      "[$n | timeSpan[±timeSpan]*]",
      "  where $j, $n ∈ NaturalNumber",
      "  and timeSpan ::= [$j.]dateAttr",
      "  and dateAttr ::= y[ear] | mon[th] | w[eek] ",
      "                |   d[ay] | h[our]  | min[ute]"
    ],

    examples: [
      "# first 5 entries in baz",
      "yorn baz --first 5",
      "# select entries in the period between the first",
      "# entry in pom and 2 months after the entry",
      "# very similiar to --last",
      "yorn pom --first 2.mon"
    ]
  },

  query: {
    syntax: [ "$year[/$month[/$day[/$hour[/$minute]]]]",
              "  where $year,$month,$day,$hour and $minute",
              "  ::= int[,(int | int-int)]* | \"@\"",
              "  $month can be any month name as well" ],
    examples: [
      "# select all entries in the 'ter' yornal (default/automatic)",
      "yorn ter --query @",
      "# select all entries in any year where the month is august",
      "yorn hue --query @/aug",
      "# selects all entries even though day was specified",
      "# because querying only cares about yornal set fields",
      "# i.e. only the two @'s are looked at in this case",
      "yorn monthlyJournal --query @/@/1"
    ]
  },

  edit: {
    default: "tail",
    syntax: [
      "loc[±$n | ±$k[±$i.dateAttr]*]",
      "  where $n, $k, $i ∈ NaturalNumber",
      "    and loc ::= t[ail] | h[ead] | m[id[dle]]",
      "    and dateAttr ::= y[ear] | mon[th]",
      "                  | d[ay] | h[our] | min[ute]"
    ],

    examples: [
      "# get entries from last year, edit the third from last entry",
      "yorn yup -l y -e t-2",
      "# error, as there is nothing after tail",
      "yorn jan -e tail+1",
      "# get all entries in hex yornal",
      "# edit entry 1.5 months before the fourth entry",
      "# entry won't exist, so will be created, and will be new first entry",
      "yorn hex -e head+3-2.month+15.day"
    ]
  },

  add: {
    syntax: [
      "$year[/$month[/$day[/$hour[/$minute]]]]",
      "  where all ∈ NaturalNumber",
      "  and $month =~ month name"
    ],

    examples: [
      "# adds 2022 entry to yearYornal, ignores fields not applicable",
      "yorn yearYornal -a 2022/aug/20"
    ]
  },

  match: {
    syntax: "word",
    examples: [
      "# select entries in last 12 years",
      "# that have the word \"money\" in it  }",
      "yorn foo -l 12.y -m money"
    ]
  },

  regex: {
    syntax: "$rubyRegex",
    examples: [
      "# select entries that have integers",
      "yorn foo -q @ -r \"^\\d+$\""
    ]
  },

  create: {
    syntax: "$yornalName",
    examples: [
      "# create day yornal named foo",
      "yorn -c foo",
      "# create box yornal named foo/bar",
      "yorn -c foo/bar -t box",
      "# create minute yornal named qux",
      "yorn -c qux -t min"
    ]
  },

  type: {
    default: "day",

    syntax: [
      "y[ear[ly]] | mon[th[ly]] | w[eek[ly]]",
      "(d|day|daily) | h[our[ly]] | m[inute[ly]]",
      "s[econd[ly]] | box"
    ],

    examples: []
  },

  print: {
    syntax: "$delimiter=\"\\n\\n\"",
    examples: [
      "# select all entries and print them without a delimiter",
      "yorn foo -q @ -p ''"
    ]
  },

  pp: {
    syntax: "$delimiter=\"\\n\" ; 'print path'",
    examples: [
      "# select last 3 entries and print paths",
      "# --print-path not needed as its default action",
      "yorn foo -l 3 --print-path"
    ]
  }
}

opts = Optimist::options do
  def documentation(option)
    $options[option][:syntax]
      .then(&Format.m(:syntax))
  end

  $options.each do |option ,hash|
    opt option, documentation(option), :type => :string, :default => hash[:default]
  end

  opt :delete, "Delete selected entries"
  opt :usage, "Print example flag usage", :type => :string
  opt :verbose, "Print more information"

  $options.each_key do |option|
    [[:add], [:create, :type]]
      .each { |set| conflicts set[0], option unless set[1..].any? option }
    conflicts :edit, option unless [:edit, :last, :first, :query].any? option
  end

  conflicts :last, :first
  conflicts :match, :regex
  conflicts :print, :pp
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

p opts
