#!/usr/bin/env ruby1.9

class TestClass
    attr_accessor :a, :b

    def initialize(a = 0, b = 0)
        @a = a
        @b = b
    end

    def display
        puts @b
    end
end

t = TestClass.new
t.a = 12
t.b = 14
puts t.a
t.display
