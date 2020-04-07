class Greeter
  attr_accessor :name

  def initialize(name = "World")
    @name = name
  end

  def sayHi
    puts "Hi #{@name.capitalize}!"
  end

  def sayBye
    pts "Bye #{@name}, come back soon."
  end
end

# greeter = Greeter.new("Cajetan")
# greeter.sayHi
# greeter.sayBye
# puts greeter.name

# puts ""

# greeter.name = "Harvey"
# greeter.sayHi
# greeter.sayBye
# puts greeter.name

class MegaGreeter
  attr_accessor :names

  # Constructor
  def initialize(names = "World")
    @names = names
  end

  # Say hi to everybody
  def sayHi
    if @names.nil?
      puts "..."
    elsif @names.respond_to?("each")
      # @names is a list of some kind, let's iterate
      @names.each do |name|
        puts "Hello #{name}!"
      end
    else
      puts "Hello #{@names}!"
    end
  end

  def sayBye
    if @names.nil?
      puts "..."
    elsif @names.respond_to?("join")
      # join the list elements with commas
      puts "Goodbye #{@names.join(", ")}. Come back soon!"
    else
      puts "Goodbye #{@names}. Come back soon!"
    end
  end
end

# if it's a main file
if __FILE__ == $0
  mg = MegaGreeter.new
  mg.sayHi
  mg.sayBye

  puts ""

  # Change the name to be "Cajetan"
  mg.names = "Cajetan"
  mg.sayHi
  mg.sayBye

  puts ""

  # Change the name to an array of names
  mg.names = ["Albert", "Brenda", "Charles",
              "Dave", "Engelbert"]
  mg.sayHi
  mg.sayBye

  puts ""

  # Change to nil
  mg.names = nil
  mg.sayHi
  mg.sayBye
end

