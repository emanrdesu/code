require 'curses'
require 'thread'
require 'optimist'
require_relative 'sort'

class DelayedArray < Array
  attr_reader :delay

  def initialize(array, delay)
    @delay = delay
    super()
    concat(array)
  end

  def [](index, skip = false)
    sleep(@delay / 1000.0) unless skip
    super(index)
  end

  def []=(index, value)
    $current = index
    sleep(@delay / 1000.0)
    super(index, value)
  end
end


# milliseconds
TIMEOUT = 5
PADDING = 1

COLOR = Hash[
  [ "green",
    "red",
    "yellow",
    "magenta",
    "blue"
  ].map.with_index do |c, i|
    [c, i + 1]
  end
]

class Display
  def self.init
    Curses.init_screen
    Curses.noecho
    Curses.raw
    Curses.timeout = TIMEOUT
    Curses.curs_set(0)
    Curses.start_color
    Curses.use_default_colors

    COLOR.each do |color, index|
      Curses.init_pair(index, Curses.const_get("COLOR_#{color.upcase}"), Curses::COLOR_BLACK)
    end
      .transform_keys! { |color| color.to_sym }
      .transform_values! { |n| Curses.color_pair(n) }

    # global terminal dimensions
    $height = { screen: Curses.lines, max: Curses.lines - 5 }
    $width = { screen: Curses.cols }

    # maps array indices to old bar height
    $clear = {}

    # maps array indices to boolean
    $colored = {}
  end

  def self.text(string, y, x)
    if x == :center
      x = ($width[:screen] - string.size) / 2
    end
    Curses.setpos(y, x)
    Curses.addstr(string)
  end

  def self.colorized(color)
    Curses.attron(COLOR[color]) if color
    yield
    Curses.attroff(COLOR[color]) if color
  end

  def self.info
    Display.colorized(:yellow) do
      $algorithm.info.each_with_index do |(key, value), i|
        Display.text("#{key}: #{value}", i + PADDING, :center)
      end
    end
  end

  def self.clear
    $clear.each do |i, last|
      now = $height[:bar][i]
      (now + 1 .. last).each do |dy|
        y = $height[:screen] - PADDING - dy
        x = i + $width[:padding]

        Display.text(" ", y, x)
      end
    end

    $clear.clear
  end

  def self.bar(value, i, force: nil)
    value = value * $height[:max] / $max

    last = $height[:bar][i]
    $clear[i] = last if value < last

    if force || value > last
      start = force ? 0 : last
      (start .. value).each do |dy|
        y = $height[:screen] - PADDING - dy
        x = i + $width[:padding]

        Display.text(?█, y, x)
      end
    end

    $height[:bar][i] = value
  end

  def self.array
    $colored[:new] = {}

    $array.each_index do |i|
      value = $array[i, true]
      color = { $min => :green, $max => :red, $current => :blue }
      $colored[:new][i] = true if color[value]

      Display.colorized(color[value]) do
        Display.bar(value, i, force: $colored[i] || color[value])
      end
    end

    $colored = $colored[:new]
  end

  def self.iteration(timeout)
    Display.clear
    Display.info
    Display.array
    Curses.refresh
    sleep(timeout)
  end

  def self.visualize
    while $threads[:sorting].alive?
      Display.iteration(TIMEOUT / 800.0)
    end

    # cleanup
    Display.iteration(TIMEOUT)
    Curses.clear
  end
end




def die(status = 0)
  yield if block_given?
  exit(status)
end

def main
  opts = Optimist.options do
    opt :delay, "Delay in milliseconds", type: :int, default: 2
    opt :algorithm, "Sorting algorithm to use", type: :string
    opt :test, "Run test mode", type: :boolean, default: false
  end

  if ARGV.size == 2
    if (ARGV[0] =~ /^\d+$/) # make sure it's an integer
      opts[:delay] = ARGV[0].to_i
    else
      $stderr.puts "Delay must be an integer"
      exit 1
    end

    opts[:algorithm] = ARGV[1]
  end


  die(1) {
    $stderr.puts "Delay must be greater than 0"
  } if opts[:delay] < 0

  die(1) {
    $stderr.puts "Invalid algorithm"
    $stderr.puts "Available algorithms: #{Sorting.algorithm.keys.map(&:to_s)}"
  } if opts[:algorithm] &&
       !Sorting.algorithm.keys.include?(opts[:algorithm])

  die {
    Sorting.algorithm.values.sort.reverse.each do |algorithm|
      print algorithm.report + "\n\n"
    end
  } if opts[:test]




  Display.init

  size = $width[:screen] - 7
  $array = DelayedArray.new((5..size + 5).to_a.shuffle, opts[:delay])
  $current = 0 ; $min = $array.min ; $max = $array.max

  $width[:padding] = ($width[:screen] - $array.size) / 2
  $height[:bar] = Array.new($array.size, 0)

  name = opts[:algorithm] || Sorting.algorithm.keys.sample
  $algorithm = Sorting.algorithm[name]

  $threads = {
    visualization: Thread.new { Display.visualize },
    sorting: Thread.new { $algorithm.sort($array) }
  }

  # main input handling thread
  loop do
    case Curses.getch
    when ?q, 3 # q, Control+D
      die {
        $threads[:sorting].kill
        $threads[:visualization].kill
      }
    end

    exit unless $threads[:visualization].alive?
  end

ensure
  Curses.clear
  Curses.close_screen
end

main
