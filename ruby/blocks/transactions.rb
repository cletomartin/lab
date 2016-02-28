require 'socket'

def open_html(url)
  s = TCPSocket.new url, 80
  s.puts 'GET /'
  while line = s.gets
    yield line
  end
  s.close()
end

open_html('google.com') do |line|
  puts line
end
