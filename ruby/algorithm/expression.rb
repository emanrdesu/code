MAP = {
  'Variable' => { 'n' => 10 },
  'Constant' => { 'e' => Math::E, 'π' => Math::PI, 'φ' => 1.618034 },

  'Operator' => {
    '+' => ->(a, b) { a + b },
    '-' => ->(a, b) { a - b },
    '*' => ->(a, b) { a * b },
    '/' => ->(a, b) { a / b },
    '^' => ->(a, b) { a**b },
    '%' => ->(a, b) { a % b }
  },

  'Function' => {
    'O' => ->(x) { x },
    'log10' => ->(x) { Math.log(x, 10) },
    'log' => ->(x) { Math.log(x, 2) },
    'ln' => ->(x) { Math.log(x) },
    '√' => Math.method(:sqrt),
    '!' => ->(x) { (1..x).inject(1, :*) },
    'sin' => Math.method(:sin),
    'cos' => Math.method(:cos),
    'tan' => Math.method(:tan)
  }
}

REGEX = MAP.keys.each_with_object({}) do |key, hash|
  strings = MAP[key].keys
  hash[key] = Regexp.new("^(#{strings.map { |s| Regexp.escape(s) }.join('|')})$")
end

REGEX['Number'] = /\A[+-]?\d+(?:\.\d+)?\z/

class Term
  def initialize(string)
    @string = string
    self.class.setmap
  end

  def evaluate
    raise NotImplementedError
  end

  def self.setmap
    name = self.name.downcase
    class_variable_set("@@#{name}", MAP[self.name])
  end
end

class Variable < Term
  def evaluate
    @@variable[@string]
  end
end

class Constant < Term
  def evaluate
    @@constant[@string]
  end
end

class Number < Term
  def evaluate
    @string.to_f
  end
end

class Operator < Term
  attr_accessor :operand

  def initialize(string, a, b)
    super(string)
    @operand = { 1 => a, 2 => b }
  end

  def evaluate
    @@operator[@string].call(@operand[1].evaluate, @operand[2].evaluate)
  end
end

class Function < Term
  attr_accessor :operand

  def initialize(string, operand)
    super(string)
    @operand = operand
  end

  def evaluate
    @@function[@string].call(@operand.evaluate)
  end
end

class Expression
  def initialize(expression)
    @expression = expression
  end

  def lexify
    stack = tokens.map { |t| term t }

    while stack.size > 2
      a = stack.shift
      op = stack.shift
      b = stack.shift

      op.operand[1] = a
      op.operand[2] = b

      stack.unshift(op)
    end

    stack[0]
  end

  def evaluate
    lexify.evaluate
  end

  private

  def tokens
    tokens = []
    token = []
    level = 0

    protocol = {
      ' ' => proc do
        if level == 0
          tokens << token.join
          token = []
        else
          token << ' '
        end
      end,

      '(' => proc { level += 1 },
      ')' => proc { level -= 1 }
    }

    @expression.chars.each do |c|
      protocol[c]&.call()
      token << c unless c == ' '
    end

    tokens.append(token.join).compact
  end

  def term(token)
    case token
    when /^\((.*)\)$/ # expression
      Expression.new(::Regexp.last_match(1)).lexify
    when /^([^(]+)\((.*)\)$/ # function call
      function = ::Regexp.last_match(1)
      operand = ::Regexp.last_match(2)
      Function.new(function, Expression.new(operand).lexify)
    when REGEX['Operator']
      Operator.new(token, nil, nil) # we'll set the operands later
    when REGEX['Constant']
      Constant.new(token)
    when REGEX['Variable']
      Variable.new(token)
    when REGEX['Number']
      Number.new(token)
    end
  end
end

# Test cases
def test_evaluate
  tests = {
    'O(e)' => Math::E,
    'O(π)' => Math::PI,
    'O(φ)' => 1.618034,
    'O(log(8))' => Math.log(8, 2),
    'O(ln(e))' => Math.log(Math::E),
    'O(√(4))' => Math.sqrt(4),
    'O(√(n))' => Math.sqrt(10),
    'O(2 ^ 3)' => 2**3,
    'O(n ^ 2)' => 1000**2,
    'O(!(3))' => (1..3).inject(1, :*),
    'O(!(n))' => (1..10).inject(1, :*),
    'O(e + π)' => Math::E + Math::PI,
    'O(π * φ)' => Math::PI * 1.618034,
    'O(log(8) + ln(e))' => Math.log(8, 2) + Math.log(Math::E),
    'O(√(4) + √(4 * 4))' => Math.sqrt(4) + Math.sqrt(4 * 4),
    'O(2 ^ 3 * n)' => 2**3 * 10
  }.map do |k, v|
    { input: k, expected: v }
  end

  tests.each do |test|
    input = test[:input]
    expected = test[:expected]
    actual = Expression.new(input).evaluate
    failed = expected != actual

    puts "Input: #{input}"
    puts "Expected: #{expected}"
    puts "Actual: #{actual}"
    puts ''
  end
end
