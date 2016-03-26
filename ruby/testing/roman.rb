class Roman
  SYMBOLS = [
    ['M', 1000],
    ['CM', 900],
    ['D', 500],
    ['C', 100],
    ['XC', 90],
    ['L', 50],
    ['X', 10],
    ['IX', 9],
    ['V', 5],
    ['I', 1],
  ]

  def initialize(value)
    fail 'Value should be an Integer' unless value.is_a? Integer
    fail 'Value should be between [1-3999]' unless (1..3999).include? value
    @value = value
  end

  def to_s
    str = ''
    current = @value
    SYMBOLS.each_with_index do |symbol, i|
      s, v = symbol
      d, r = current.divmod(v)
      next if d.zero? && r == v

      if d <= 3
        str << s * d
      else
        str << s + (SYMBOLS[i-1][0] * (d - 3))
      end
      current = r
    end
    str
  end
end
