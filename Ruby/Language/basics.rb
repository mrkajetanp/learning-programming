# frozen_string_literal: true

puts 'test'

puts Math.sqrt(9)

def hi(name = 'world')
  puts "Hello #{name.capitalize}"
end

def hello
  puts 'test'
end

hi('you')
hi
hi('me')

# Here goes
# Class description ...
class Greeter
  def initialize(name = 'World')
    @name = name
  end

  def say_hi
    puts "Hi #{@name}!"
  end

  def say_bye
    puts "Bye #{@name}!"
  end
end

g = Greeter.new('Kajetan')
g.say_hi
g.say_bye
