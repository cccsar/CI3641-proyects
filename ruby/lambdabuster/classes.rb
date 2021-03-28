require 'date'
require 'set'
require 'json'

class SearchList

    # Cannot modify, just get
    attr_reader :list

    # init with specified arguments as only members
    def initialize(*elems)
        @list = []
        for elem in elems
            @list << elem
        end
    end

    # Add one element
    def <<(elem)
        @list<< elem
        self
    end

    # return a string representation 
    def to_s
        return @list.join(", ")
    end

    # Add this search list to other one
    def +(other )
        SearchList.new(@list + other.list)
    end

    def each (* args , &block )
        @list.each(* args , &block )
    end

    # Scan this searchlist looking for 
    # properties that match
    def scan (atom, &block)
      begin
        newlist = @list.filter {|x| block.call(x.send(atom))}
        return SearchList.new(*newlist)
      rescue NoMethodError
          puts "Ese atomo no existe"
      end
    end
end


#######################################3

class Person

  attr_reader :name, :birthday, :nationality
  def initialize (name,birthday,nationality) 
    @name = name
    @birthday = birthday
    @nationality = nationality
  end

  def to_s
    #"Debugging ... name: #{@name} | birthday: #{@birthday} | nationality #{@nationality} " 
    "#{name} #{birthday} #{nationality}"
  end

end

class Actor 

  attr_reader :starred_in, :name, :birthday, :nationality
  attr_writer :starred_in

  def initialize (person, starred_in = SearchList.new() ) 
    @person = person
    @starred_in = starred_in
  end

  def name
    @person.name
  end

  def birthday
    @person.birthday
  end

  def nationality
    @person.nationality
  end

  def to_s
    @person.to_s + "| starred in: #{@starred_in.list.map{|mv| mv.name}.join(", ")}" # only names of movies
  end

end 

class Director 

  attr_reader :directed, :name, :birthday, :nationality
  attr_writer :directed 

  def initialize (person, directed = SearchList.new() ) 
    @person = person
    @directed = directed
  end

  def name
    @person.name
  end

  def birthday
    @person.birthday
  end

  def nationality
    @person.nationality
  end

  def to_s
    @person.to_s + "| directed: #{@directed.list.map{|mv| mv.name}.join(", ")}" # only names of movies
  end

end 

#######################################3

class Movie
  attr_reader :name, :runtime, :categories, :release_date, :directors, :actors, :price, :rent_price

  def initialize( name = "", 
            runtime = 0, 
            categories = Set.new(), 
            release_date = Date.today, 
            directors = SearchList.new(), 
            actors = SearchList.new(), 
            price = 0.0, 
            rent_price = 0.0
          )
            
    @name = name
    @runtime = runtime
    @release_date = release_date
    @directors = directors
    @actors = actors
    @price = price
    @rent_price = rent_price
    @categories = categories
  end # como garantizo que se incluya la pelicula en starred_in o directed de actor y director respectivamente?
  
  def to_s 
    hours = @runtime / 60
    mins  = @runtime % 60 
    
    cat_arr = @categories.to_a
    # build category string
    if cat_arr.length == 1
      categories = @categories[0]
    elsif cat_arr.empty?
      categories = "No genres available"
    else
      categories = cat_arr[0..cat_arr.length - 2].join(", ") # everything but the last
      categories += " and #{cat_arr.last}"
    end

    dir_names = @directors.list.map{ |dir| dir.name }.join(", ") 
    act_names = @actors.list.map { |act| act.name }.join(", ") 

    # Build line by line with string interpolation
    l1 = "\t#{@name} (#{@release_date}) - Hours #{hours} Mins #{mins}\n"
    l2 = "\tGenres: #{categories}\n"
    l3 = "\tDirected by: #{dir_names}\n" #"\tDirected by: #{@directors}\n"
    l4 = "\tCast: #{act_names}" # "\tCast: #{@actors}"

    return l1 + l2 + l3 + l4 
  end

end


class Premiere

  attr_reader :name, :runtime, :categories, :release_date, :directors, :actors, :price, :rent_price

  def initialize(movie)
    @movie = movie
  end

  def to_s
    @movie.to_s 
  end

  def price 
    @movie.price * 2
  end

  def name
    @movie.name
  end

  def runtime
    @movie.runtime
  end

  def categories
    @movie.categories
  end

  def release_date
    @movie.release_date
  end

  def directors
    @movie.directors
  end

  def actors
    @movie.actors
  end

  def rent_price
    @movie.rent_price * 2
  end

end

class Discount

  attr_reader :name, :runtime, :categories, :release_date, :directors, :actors, :price, :rent_price

  def initialize(movie, discount)
    @movie = movie
    @discount = discount
  end

  def to_s
    @movie.to_s 
  end

  def price 
    @movie.price - @discount
  end

  def name
    @movie.name
  end

  def runtime
    @movie.runtime
  end

  def categories
    @movie.categories
  end

  def release_date
    @movie.release_date
  end

  def directors
    @movie.directors
  end

  def actors
    @movie.actors
  end

  def rent_price
    @movie.rent_price - @discount
  end
end


#######################################3

# Transforming nums into currency
class Integer
  def dolars
    Dolar.new(self)
  end

  def euros
    Euro.new(self)
  end

  def bolivares
    Bolivar.new(self)
  end

  def bitcoins
    Bitcoin.new(self)
  end
end

class Float
  def dolars
    Dolar.new(self)
  end

  def euros
    Euro.new(self)
  end

  def bolivares
    Euro.new(self)
  end

  def bitcoins
    Bitcoin.new(self)
  end
end

class Currency

  attr_reader :value
  def initialize(value)
    @value = value + 0.0
    @@CURRENCY_TYPES = {
      :bolivar => Bolivar,
      :dolar   => Dolar,
      :euro    => Euro,
      :bitcoin => Bitcoin
    }
  end

  def in atom
    #  currency we want to change to
    target = @@CURRENCY_TYPES[atom]
    #  value in dolars
    in_dolars = self.to_dolar
    

    #puts "value in dolar: #{in_dolars}"

    # Convert anything 
    target.from_dolar(in_dolars)
  end

  # so we can convert to dolar and then to our desired corrency
  def to_dolar
    throw NotImplementedError
  end

  def self.from_dolar(dolars)
    throw NotImplementedError
  end

  # comparing currencies
  def compare(other)
    self_value = self.to_dolar.value
    other_value = other.to_dolar.value

    if self_value < other_value
      return :lesser
    elsif self_value == other_value
      return :equal
    else
      return :greater
    end
    
  end
end

class Dolar < Currency

  def to_s
    "#{@value} $"
  end

  def to_dolar
    Dolar.new(@value)
  end 

  def self.from_dolar(dolars)
    Dolar.new(dolars.value)
  end
end

class Euro < Currency

  def to_s
    "#{@value} €"
  end

  def self.from_dolar(dolars)
    Euro.new(dolars.value / 1.18)
  end

  def to_dolar 
    Dolar.new(@value * 1.18)
  end
end

class Bolivar < Currency

  def to_s
    "#{@value} Bs"
  end

  def self.from_dolar(dolars)
    Bolivar.new(dolars.value * 1825348.0)
  end

  def to_dolar
    Dolar.new(@value / 1825348.0)
  end   
end

class Bitcoin < Currency

  def to_s
    "#{@value} BTC"
  end

  def self.from_dolar(dolars)
    Bitcoin.new(dolars.value / 52.009)
  end 

  def to_dolar
    Dolar.new(@value * 52.009)
  end

end


#######################################3

module BuyOrder
  def buy_order
    self.date = Date.today
    self.total = self.movie.price
  end
end

module RentOrder
  def rent_order
    self.date = Date.today
    self.total = self.movie.rent_price
  end
end

class Transaction

  include BuyOrder
  include RentOrder

  attr_reader :movie, :type, :total, :date
  attr_writer :total, :date

  def initialize (movie, type) 
    # store name and type
    @movie = movie
    @type = type

    # check transaction type
    if type == :buy
      self.buy_order
    elsif type == :rent
      self.rent_order
    else 
      raise "Este no es un tipo válido de transacción: #{type}"
    end
  end

end

#######################################3

class User
  attr_reader :owned_movies, :rented_movies, :transactions

  def initialize (owned_movies = SearchList.new(), rented_movies= SearchList.new(), transactions= SearchList.new()) 
    @owned_movies  = owned_movies
    @rented_movies = rented_movies
    @transactions  = transactions
  end
    
end

########################################

#Client

class Client
  attr_reader :known_persons, :known_actors, :known_directors, :movie_catalog, :existing_categories, :user
  
  def initialize (known_persons = Map.new(), 
                  known_actors = Map.new(),
                  known_directors = Map.new(),
                  movie_catalog = SearchList.new(),
                  existing_categories = Set.new(),
                  user = User.new()
                 ) 
    @known_persons       = known_persons       # map of person by name 
    @known_actors        = known_actors        # map of actors by name, coungrent with known persons
    @known_directors     = known_directors     # map of directors by name, coungrent with known persons
    @movie_catalog       = movie_catalog       # SearchList of movies
    @existing_categories = existing_categories # set of movie categories
    @user                = user                # a user
  end

end



=begin
 # proof that Discount and Premiere are 'composable' 
movie_catalog.each do 
  |peli| 
  a = Premiere.new( Discount.new( peli, 1 ) )
  puts "price: #{a.price} | rent_price: #{a.rent_price}"
end
=end

=begin
 # proof that ruby doesn't believe in no one
 a = Person.new("a","a","a")
 a = Actor.new(a)
 puts a 
 => it returns what was requested
=end

=begin
puts "Persons information:" 
persons_map.each do 
  |_,person|
  puts "\tname: #{person.name}, bday: #{person.birthday}, nat: #{person.nationality}"
end

puts "Actors information:" 
actors_map.each do 
  |_,actor|
  puts "\tname: #{actor.name}, bday: #{actor.birthday}, nat: #{actor.nationality}"
  #puts "\tstarred in: #{actor.starred_in} " 
end

puts "Directors information:"
directors_map.each do 
  |_,director|
  puts "\tname: #{director.name}, bday: #{director.birthday}, nat: #{director.nationality}"
  #puts "\tdirected: #{director.directed}" 
end

puts "Movie catalog" 
movie_catalog.each do 
  |movie|
  puts "\tname: #{movie.name}, runtime: #{movie.runtime}, price: #{movie.price}, rent_price: #{movie.rent_price}"
  puts "\tcategories:"
  movie.categories.each { |cat| puts "\t\t#{cat}" }
  puts "\tactors:"
  puts "\t\t #{movie.actors}" 
  puts "\tdirectors:" 
  puts "\t\t #{movie.directors}" 
end

puts "Categories"
categories_set.each{ |cat| puts "\t#{cat}"} 
=end
