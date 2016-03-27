require 'fileutils'

class LogConverter
  ROWS = 49

  def convert(input)
    output_content = []
    input.split("\n").each_with_index do |l, i|
      line = i / ROWS
      output_content << [] if output_content.length == line
      output_content[line] += (l.strip.gsub(/"/, '').split(','))
    end
    output_content.map{ |l| l.join(',') }.join("\n")
  end

end
