# coding: utf-8
l = lambda { |s| puts s }
l.call('test')

def prefix(p)
  lambda { |s| puts "#{p}#{s}"}
end

prefix('---> ').call('test')

l = ->(s) { puts s }
l.call('test')
