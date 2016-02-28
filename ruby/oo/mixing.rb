# -*- coding:utf-8 -*-

module Test
  def to_s_with_stars(n=3)
    "#{'*' * n} #{self.to_s} #{'*' * n}"
  end
end

class Person
  include Test

  def initialize(name, age)
    @name = name
    @age = age
  end

  def to_s
    "#{@name} is #{@age} years old"
  end
end

p = Person.new('Ujier Chiquitico', 5)
puts p.to_s
puts p.to_s_with_stars
puts p.to_s_with_stars(10)
