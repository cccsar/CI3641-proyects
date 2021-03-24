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

  def initialize (name,birthday,nationality) 
    @name = name
    @birthday = birthday
    @nationality = nationality
  end

  def to_s 
    @name
  end
end

class Actor < Person

  attr_reader :stareed_in

  def initialize (name, birthday, nationality, starred_in = SearchList.new() ) 
    @name = name
    @birthday = birthday 
    @nationality = nationality
    @starred_in = starred_in
  end

end # como garantizo que exista la instancia de Persona con los atributos que se derivan?


class Director < Person
   def initialize (name, birthday, nationality, directed = SearchList.new() ) 
    @name = name
    @birthday = birthday 
    @nationality = nationality
    @directed = directed
  end
end #como garantizo que esxista la instancia de Persona con los atributos que se derivan?


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
            rent_price = 0.0 )
            
    @name = name
    @runtime = runtime
    @release_date = release_date
    @directors = directors
    @actors = directors
    @price = price
    @rent_pric = rent_price
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
end

class Discount 
end


#######################################3

class Currency
  def in atom
  end

end

class Dolar < Currency
  def in atom
  end
end

class Euro < Currency
  def in atom
  end
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
