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
        raise NotImplementedError
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
end


#######################################3

class Person

  def initialize (name,birthday,nationality) 
    @name = name
    @birthday = birthday
    @nationality = nationality
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

  def initialize(name, runtime, categories = [], release_date, directors, actors, price, rent_price)
    @name = name
    @runtime = runtime
    @release_date = release_date
    @directors = SearchList.new()
    @actors = SearchList.new()
    @price = price
    @rent_pric = rent_price
  end # como garantizo que se incluya la pelicula en starred_in o directed de actor y director respectivamente?
  
  def to_s 
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

class Transaction

  include BuyOrder
  include RentOrder

  attr_reader :movie, :type, :total, :date

  def initialize (movie, type) 
    @movie = movie
    @type = type
  end

end

module BuyOrder
  def buy_order
  end
end

module RentOrder
  def rent_order
  end
end


#######################################3

class User
  attr_reader :owned_movies, :rented_movies, :transactions

  def initialize (owned_movies = SearchList.new(), rented_movies= SearchList.new(), transactions= SearchList.new()) 
  end
    
end
