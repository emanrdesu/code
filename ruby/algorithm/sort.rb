require 'benchmark'
require_relative "expression"

module Sorting
  class Algorithm
    attr_reader :name, :time, :space, :sort

    def initialize(name, time, space, sort)
      @name = name
      @time = time
      @space = space
      @sort = sort
    end

    def sort(*args)
      @sort.call(*args)
    end

    def info
      {
        "Algorithm"        => @name,
        "Time Complexity"  => @time,
        "Space Complexity" => @space
      }
    end

    def report
      info.map { |k, v| "#{k}: #{v}" }.join("\n")
    end

    def <=>(other)
      comparison = { time: evaluate(@time) <=> evaluate(other.time) }
      return comparison[:time] if comparison[:time] != 0

      comparison[:space] = evaluate(@space) <=> evaluate(other.space)
      return comparison[:space] if comparison[:space] != 0

      benchmark(other)
    end

    def test(size = 1000)
      array = Sorting.randomArray(size)
      original = array.clone

      start_time = Time.now
      sort(array)
      end_time = Time.now

      puts report
      puts "Time taken: #{end_time - start_time} seconds\n\n"

      array == original.sort
    end

    private

    def evaluate(complexity)
      Expression.new(complexity).evaluate
    end

    def benchmark(other)
      array = Sorting.randomArray(100)
      copy = array.dup
      margin = 0.01

      time = {
        other: Benchmark.measure { other.sort(copy) },
        self:  Benchmark.measure { sort(array) }
      }

      time.transform_values! { |v| v.real }

      if (time[:self] - time[:other]).abs < margin
        return 0
      elsif time[:self] < time[:other]
        return -1
      else
        return 1
      end
    end
  end


  def self.randomArray(size)
    Array.new(size) { rand(size) }
  end

  def self.testall
    algorithm.values.each do |algorithm|
      puts "Testing #{algorithm.name}:"
      puts algorithm.test
      puts
    end
  end

  # dictionary mapping algorithm names to their instances
  def self.algorithm
    {
      selection: Algorithm.new(
        "Selection Sort",
        "O(n ^ 2)",
        "O(1)",
        -> (array) {
          n = array.length
          (0...n).each do |i|
            mini = i

            (i + 1...n).each do |j|
              mini = j if array[j] < array[mini]
            end

            array[i], array[mini] = array[mini], array[i] if mini != i
          end
        }
      ),

      insertion: Algorithm.new(
        "Insertion Sort",
        "O(n ^ 2)",
        "O(1)",
        -> (array) {
          n = array.length
          (1...n).each do |i|
            key = array[i]
            j = i - 1
            while j >= 0 && array[j] > key
              array[j + 1] = array[j]
              j -= 1
            end
            array[j + 1] = key
          end
        }
      ),

      quick: Algorithm.new(
        "Quick Sort",
        "O(n * log(n))",
        "O(n)",
        -> (array, low = 0, high = array.length - 1) {
          partition = ->(low, high) do
            pivot = array[high]
            i = low - 1

            (low...high).each do |j|
              if array[j] <= pivot
                i += 1
                array[i], array[j] = array[j], array[i]
              end
            end

            # Place pivot in correct position
            array[i + 1], array[high] = array[high], array[i + 1]
            i + 1
          end

          if low < high
            this = Sorting.algorithm["quick"]

            pivot = partition.(low, high)
            this.sort(array, low, pivot - 1)
            this.sort(array, pivot + 1, high)
          end

        }
      ),


      heap: Algorithm.new(
        "Heap Sort",
        "O(n * log(n))",
        "O(1)",
        -> (array) {
          heapify = ->(arr, n, i) {
            largest = i
            left = 2 * i + 1
            right = 2 * i + 2

            largest = left if left < n && arr[left] > arr[largest]
            largest = right if right < n && arr[right] > arr[largest]

            if largest != i
              arr[i], arr[largest] = arr[largest], arr[i]
              heapify.(arr, n, largest)
            end
          }

          n = array.length
          (n / 2 - 1).downto(0) do |i|
            heapify.(array, n, i)
          end

          (n - 1).downto(1) do |i|
            array[0], array[i] = array[i], array[0]
            heapify.(array, i, 0)
          end
        }
      ),


      counting: Algorithm.new(
        "Counting Sort",
        "O(n)",
        "O(1)",
        -> (array) {
          return array if array.empty?

          max = array.max
          count = Array.new(max + 1, 0)

          array.each do |num|
            count[num] += 1
          end

          index = 0
          count.each_with_index do |c, i|
            c.times { array[index] = i; index += 1 }
          end
        }
      ),


      radix: Algorithm.new(
        "Radix Sort",
        "O(n)",
        "O(n)",
        -> (array) {
          return array if array.empty?

          max_value = array.max
          exp = 1

          digitCountSort = ->(arr, exp) {
            n = arr.length
            output = Array.new(n)
            count = Array.new(10, 0)

            arr.each do |num|
              index = (num / exp) % 10
              count[index] += 1
            end

            (1...10).each do |i|
              count[i] += count[i - 1]
            end

            (n - 1).downto(0) do |i|
              index = (arr[i] / exp) % 10
              output[count[index] - 1] = arr[i]
              count[index] -= 1
            end

            output.each_with_index do |value, index|
              arr[index] = value
            end
          }

          while max_value / exp > 0
            digitCountSort.call(array, exp)
            exp *= 10
          end
        }
      ),


      shell: Algorithm.new(
        "Shell Sort",
        "O(n * log(n))",
        "O(1)",
        -> (array) {
          n = array.length
          gap = n / 2

          while gap > 0
            (gap...n).each do |i|
              temp = array[i]
              j = i

              while j >= gap && array[j - gap] > temp
                array[j] = array[j - gap]
                j -= gap
              end
              array[j] = temp
            end
            gap /= 2
          end
        }
      ),


      bogo: Algorithm.new(
        "Bogosort",
        "O(!(n + 1))",
        "O(n)",
        -> (array) {
          sorted = -> (array) {
            (0...array.length - 1).all? { |i| array[i] <= array[i + 1] }
          }

          until sorted.(array)
            array.shuffle!
          end
        }
      ),


      comb: Algorithm.new(
        "Comb Sort",
        "O(n * log(n))",
        "O(1)",
        -> (array) {
          gap = array.length
          swapped = true

          while gap > 1 || swapped
            gap = (gap / 1.3).floor
            gap = 1 if gap < 1
            swapped = false

            (0...(array.length - gap)).each do |i|
              if array[i] > array[i + gap]
                array[i], array[i + gap] = array[i + gap], array[i]
                swapped = true
              end
            end
          end
        }
      ),

      gnome: Algorithm.new(
        "Gnome Sort",
        "O(n ^ 2)",
        "O(1)",
        -> (array) {
          i = 0
          n = array.length

          while i < n
            if i == 0 || array[i] >= array[i - 1]
              i += 1
            else
              array[i], array[i - 1] = array[i - 1], array[i]
              i -= 1
            end
          end
        }
      ),


      pigeonhole: Algorithm.new(
        "Pigeonhole Sort",
        "O(n)",
        "O(n)",
        -> (array) {
          min = array.min
          max = array.max
          range = max - min + 1
          holes = Array.new(range, 0)

          array.each do |num|
            holes[num - min] += 1
          end

          index = 0
          holes.each_with_index do |count, i|
            count.times do
              array[index] = i + min
              index += 1
            end
          end
        }
      ),

      bitonic: Algorithm.new(
        "Bitonic Sort",
        "O(log(n))",
        "O(n)",
        ->(array, low = 0, count = array.size, ascending = true) {
          return if count <= 1

          merge = ->(array, low, count, ascending) {
            return if count <= 1

            mid = count / 2
            (low...(low + mid)).each do |i|
              if (array[i] > array[i + mid]) == ascending
                array[i], array[i + mid] = array[i + mid], array[i]
              end
            end

            merge.(array, low, mid, ascending)
            merge.(array, low + mid, mid, ascending)
          }

          this = Sorting.algorithm["bitonic"]

          mid = count / 2
          this.sort(array, low, mid, true)
          this.sort(array, low + mid, mid, false)
          merge.(array, low, count, ascending)
        }
      ),


      merge: Algorithm.new(
        "Merge Sort",
        "O(n * log(n))",
        "O(n)",
        -> (array) {
          return array if array.length <= 1

          merge = ->(left, right, array) {
            i = 0
            j = 0
            k = 0

            while i < left.length && j < right.length
              if left[i] <= right[j]
                array[k] = left[i]
                i += 1
              else
                array[k] = right[j]
                j += 1
              end

              k += 1
            end

            while i < left.length
              array[k] = left[i]
              i += 1
              k += 1
            end

            while j < right.length
              array[k] = right[j]
              j += 1
              k += 1
            end

            array
          }

          this = Sorting.algorithm["merge"]

          mid = array.length / 2
          left = array[0...mid]
          right = array[mid...array.length]

          merge.(this.sort(left), this.sort(right), array)
        }
      ),

    }.transform_keys(&:to_s)
  end
end
