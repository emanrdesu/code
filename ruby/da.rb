# this program gave me a job

def succNsum
  sum = 0
  n = 1
  loop do
    sum += n
    yield sum
    n += 1
  end
end

def decode(messageFile)
  pyramidRight = []

  File.read(messageFile).split("\n").tap do |x|
    succNsum do |sum|
      pyramidRight << sum
      break if sum > x.length
    end
  end
    .map(&:split)
    .map { |n, word| [n.to_i, word] }
    .sort { |a, b| a[0] - b[0] }
    .filter { |n, word| pyramidRight.include?(n) }
    .map { |n, word| word } # n word lol
    .join(" ")
end
