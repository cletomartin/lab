require 'roman'

require 'test/unit'

class TestRoman < MiniTest::Unit::TestCase

  def setup
    p 'This is executed before each tests'
  end

  def teardown
    p 'This is executed after each tests'
  end

  def test_invalid_values
    assert_raises(RuntimeError) { Roman.new('') }
    assert_raises(RuntimeError) { Roman.new(1.4) }

    assert_raises(RuntimeError) { Roman.new(0) }
    assert_raises(RuntimeError) { Roman.new(4000) }
  end

  def test_not_nil
    refute_nil(Roman.new(100), 'Roman number is never nil')
  end

  def test_to_s_symbols
    assert_equal('I', Roman.new(1).to_s)
    assert_equal('V', Roman.new(5).to_s)
    assert_equal('X', Roman.new(10).to_s)
    assert_equal('L', Roman.new(50).to_s)
    assert_equal('C', Roman.new(100).to_s)
    assert_equal('D', Roman.new(500).to_s)
    assert_equal('M', Roman.new(1000).to_s)
  end

  def test_to_s_symbol_rules
    assert_equal('MMMII', Roman.new(3002).to_s)
    assert_equal('CIII', Roman.new(103).to_s)
    assert_equal('DCCXXI', Roman.new(721).to_s)
    assert_equal('XVI', Roman.new(16).to_s)
    assert_equal('CD', Roman.new(400).to_s)
    assert_equal('IV', Roman.new(4).to_s)
    assert_equal('VI', Roman.new(6).to_s)
    assert_equal('VII', Roman.new(7).to_s)
    assert_equal('VIII', Roman.new(8).to_s)
    assert_equal('XLIX', Roman.new(49).to_s)
    assert_equal('MMCDXCIV', Roman.new(2494).to_s)
    assert_equal('CXXVIII', Roman.new(128).to_s)
    assert_equal('CDXLIV', Roman.new(444).to_s)
    assert_equal('CMXCIX', Roman.new(999).to_s)
    assert_equal('MMMCMXCIX', Roman.new(3999).to_s)
  end
end
