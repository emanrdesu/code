require 'curses'

def rand_speed
  (rand < 0.5 ? -1 : 1) * (rand * 5 + 5)
end

def rand_velocity
  [rand_speed, rand_speed]
end

Curses.init_screen
Curses.start_color
Curses.use_default_colors
Curses.curs_set(0)
Curses.noecho
Curses.timeout = 50 # milliseconds;
Curses.init_pair(1, Curses::COLOR_GREEN, Curses::COLOR_BLACK)
Curses.init_pair(2, Curses::COLOR_MAGENTA, Curses::COLOR_BLACK)

$height, $width = Curses.lines, Curses.cols
$t0 = Time.now

class Ball
  @@balls = []

  attr_reader :y, :x, :vx, :vy, :mass, :char

  def initialize
    @y, @x = rand(0..$height - 1), rand(0..$width - 1)
    @vy, @vx = rand_velocity
    @mass = rand(1..5000000)
    @char = ['r', 'p', 's'].sample
    @@balls << self

    @thread = Thread.new do
      loop do
        dt = Time.now - $t0
        dy = @vy * dt
        dx = @vx * dt

        # y position
        if (@y + dy).between?(0, $height - 1)
          @y += dy
        else
          @vy = -@vy * 0.9
        end

        # x position
        if (@x + dx).between?(0, $width - 1)
          @x += dx
        else
          @vx = -@vx * 0.9
        end

        # gravitation / repulsion
        @@balls.each do |ball|
          next if ball == self
          r = Math.sqrt((ball.x - @x)**2 + (ball.y - @y)**2)

          if r <= 1
            if ball.char != @char
              @char = ball.char unless beats?(ball)
            end

            @vx = -@vx * 0.9
            @vy = -@vy * 0.9
          end

          if r > 1 && @char != ball.char
            f = 0.001 * ball.mass * @mass / (r**2)
            dx = ball.x - @x
            dy = ball.y - @y
            theta = Math.atan2(dy, dx)

            fx = f * Math.cos(theta)
            fy = f * Math.sin(theta)

            ay = fy / @mass
            ax = fx / @mass

            if beats?(ball)
              @vy += ay * dt
              @vx += ax * dt
            else
              @vy -= ay * dt
              @vx -= ax * dt
            end
          end
        end

        sleep 0.01
      end
    end
  end

  def kill
    @thread.kill
    @@balls.delete(self)
  end

  def beats?(other)
    (@char == 'r' && other.char == 's') ||
    (@char == 's' && other.char == 'p') ||
    (@char == 'p' && other.char == 'r')
  end
end

begin
  balls = []
  rand(1..10).times { balls << Ball.new }

  loop do
    Curses.clear

    balls.each do |ball|
      Curses.setpos(ball.y, ball.x)
      if ball.char == 's'
        Curses.attron(Curses.color_pair(1))
      elsif ball.char == 'r'
        Curses.attron(Curses.color_pair(2))
      end
      Curses.addstr(ball.char)
      Curses.attroff(Curses.color_pair(2))
      Curses.attroff(Curses.color_pair(1))
    end

    Curses.refresh

    case Curses.getch
    when 'q'
      exit
    when 'k'
      balls << Ball.new
    when 'j'
      b = balls.pop
      b.kill if b
    end

    $height, $width = Curses.lines, Curses.cols
    $t0 = Time.now
  end
ensure
  Curses.close_screen
end
