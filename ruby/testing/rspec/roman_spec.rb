require 'roman'

describe Roman, '.intialize' do

  before(:each) do
    p 'This is executed before each test'
  end

  after(:each) do
    p 'This is executed after each test'
  end

  it 'fails if a non-integer is provided' do
    expect{Roman.new('')}.to raise_error(RuntimeError)
  end

  it 'fails if an intenger outside of the range [1-3999] is provided' do
    expect{Roman.new(0)}.to raise_error(RuntimeError)
    expect{Roman.new(4000)}.to raise_error(RuntimeError)
  end
end

describe Roman, '.to_s'  do
  examples = {
    1 => 'I',
    2 => 'II',
    3 => 'III',
    4 => 'IV',
    5 => 'V',
    6 => 'VI',
    7 => 'VII',
    8 => 'VIII',
    9 => 'IX',
    10 => 'X',
    16 => 'XVI',
    49 => 'XLIX',
    103 => 'CIII',
    128 => 'CXXVIII',
    400 => 'CD',
    444 => 'CDXLIV',
    721 => 'DCCXXI',
    999 => 'CMXCIX',
    3999 => 'MMMCMXCIX',
  }

  examples.each do |input, output|
    it "returns #{input} if value is #{output}" do
      r = Roman.new(input)
      expect(r.to_s).to eq(output)
    end
  end
end
