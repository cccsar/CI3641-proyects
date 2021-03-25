require './classes.rb'


movie1 = Movie.new("Atomic Blonde", 157, [], Date.parse("2021-03-30"), SearchList.new(), SearchList.new(), 30.0, 15.0)
movie2 = Premiere.new(movie1)
movie3 = Discount.new(movie1, 3)
movie4 = Discount.new(movie2, 5)

leo1 = Person.new("Leo Dicap", Date.today, 'venekan')
leo2 = Person.new("Leo Dicap", Date.today, 'venekan')
actor = Actor.new(leo1)

puts movie1.price
puts movie2.price
puts movie3.price
puts movie4.price

puts 3.dolars.value