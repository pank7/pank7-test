#!/usr/bin/env ruby1.9

def func1
    @debug = "hohoho"
    $debug = "hahaha"
end

def func2
    @debug = "hihihi"
    $debug = "hehehe"
end

class TestClass
    @@testvar = nil
end

func2
func1
puts "#{$debug}"
@a = "shit..."
@b = "holy..."
if false
    puts "#{@a}"
elsif
    puts "#{@b}"
end
