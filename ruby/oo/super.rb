# -*- coding:utf-8 -*-

class Person
  attr_reader :name, :age
  def initialize(name, age)
    @name = name
    @age = age
  end
end

class Student < Person
  attr_reader :course
  def initialize(course, *args)
    super(*args)
    @course = course
  end
end

s = Student.new(:first, 'John Smith', 18)
p s
