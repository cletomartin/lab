require 'lib/logconverter'

describe LogConverter do
  describe 'convert' do

    let(:lc) { LogConverter.new }

    it 'returns "" if input is ""' do
      expect(lc.convert('')).to eq('')
    end

    it 'returns "1,2,3,4\n" if input is "1,2,3,4"' do
      expect(lc.convert("1,2,3,4\n")).to eq('1,2,3,4')
    end

    it 'works with real data' do
      input = "1,2,3,4\n" * (49 * 2)
      expect(lc.convert(input)).to(
        eq(
          ('1,2,3,4,' * LogConverter::ROWS)[0...-1] + "\n" +
          ('1,2,3,4,' * LogConverter::ROWS)[0...-1]
        )
      )
    end
  end
end
