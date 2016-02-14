def fibonacci
  Enumerator.new do |yielder|
    n_1 = 0
    n_2 = 1
    yielder.yield n_2
    loop do
      sum = n_1 + n_2
      yielder.yield sum
      n_1 = n_2
      n_2 = sum
    end
  end
end

f = fibonacci
100.times { puts f.next }
