require 'date'

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
        raise NotImplementedError
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
  attr_writer :name
  def initialize (name,birthday,nationality) 
    @name = name
    @birthday = birthday
    @nationality = nationality
  end

  def to_s 
    @name
  end
end

class Actor 

  attr_reader :starred_in, :name, :birthday, :nationality

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

  def starred_in
    @person.starred_in
  end

end 

class Director 

  attr_reader :starred_in, :name, :birthday, :nationality

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

  def directed
    @person.directed
  end

end 

#######################################3

class Movie
  attr_reader :name, :runtime, :categories, :release_date, :directors, :actors, :price, :rent_price

  def initialize( name = "", 
            runtime = 0, 
            categories = [], 
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
    @actors = directors
    @price = price
    @rent_price = rent_price
    @categories = categories
  end # como garantizo que se incluya la pelicula en starred_in o directed de actor y director respectivamente?
  
  def to_s 
    hours = @runtime / 60
    mins  = @runtime % 60 
    
    # build category string
    if @categories.length == 1
      categories = @categories[0]
    elsif @categories.empty?
      categories = "No genres available"
    else
      categories = @categories[0..@categories.length - 2].join(", ") # everything but the last
      categories += " and #{@categories.last}"
    end

    # Build line by line with string interpolation
    l1 = "#{@name} (#{@release_date}) - Hours #{hours} Mins #{mins}\n"
    l2 = "Genres: #{categories}\n"
    l3 = "Directed by: #{@directors}\n"
    l4 = "Cast: #{@actors}"

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
    @movie.rent_price
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
    Euro.new(self)
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
  end

  def in atom
  end
end

class Dolar < Currency
  
  def to_bs
    @value * 1825348.00  # may wildly change depending on when you run this program
  end

  def to_euro
    @value * 0.85
  end

  def to_btc
    @value * 0.000019
  end

end

class Euro < Currency
  

  
end

class Bolivar < Currency

  def in atom
  end

  def compare other
  end
end

class Bitcoin < Currency
  def in atom
  end
end


#######################################3

#class Transaction
#
#  include BuyOrder
#  include RentOrder
#
#  attr_reader :movie, :type, :total, :date
#
#  def initialize (movie, type) 
#    @movie = movie
#    @type = type
#  end
#
#end

#module BuyOrder
#  def buy_order
#  end
#end
#
#module RentOrder
#  def rent_order
#  end
#end


#######################################3

class User
  attr_reader :owned_movies, :rented_movies, :transactions

  def initialize (owned_movies = SearchList.new(), rented_movies= SearchList.new(), transactions= SearchList.new()) 
  end
    
end
