require "./classes.rb"

# -- Utilities ------------------- +
# Print prompt + information
def inform text
    puts "Lambdabuster> "+text
end

# Print prompt and ask for user input
def ask_input 
    print "Lambdabuster> "
    gets.chomp
end

# -- Main Code ------------------- +
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

    def main 
        # Objects to use across user interation.
        persons_map    = Hash.new() 
        actors_map     = Hash.new()
        directors_map  = Hash.new() 
        movie_catalog  = SearchList.new()
        categories_set = Set[]
        
        greet = "Bienvenido a Lambdabuster!"
        
        # Proper JSON read.
        while ( true ) 
        
            print "Ingrese el nombre de un archivo JSON correctamente formateado: "
            filename = gets.chomp
        
            if (File.exist?(filename)) then
        
            fObject = File.open(filename) 
            fString = fObject.read 
            fObject.close 
        
            myJson = JSON.parse(fString)
            break 
            else
            puts "No such a file named #{filename}"
            end
        
        end
        
        
        # Traversal of json and information gathering.
        
        for actor_descriptor in myJson["actors"]  # Gather actors information
            name        = actor_descriptor["name"]
            birthday    = Date.parse ( actor_descriptor["birthday"]  )
            nationality = actor_descriptor["nationality"]
        
            thisPerson = Person.new(name, birthday, nationality)  
        
            persons_map[name] = thisPerson 
            actors_map[name]  = Actor.new(thisPerson) 
        end
        
        for director_descriptor in myJson["directors"] # Gather directors information
            name        = director_descriptor["name"]
            birthday    = Date.parse( director_descriptor["birthday"]  )
            nationality = director_descriptor["nationality"]
        
            thisPerson = Person.new(name, birthday, nationality) 
        
            persons_map[name]   = thisPerson if !persons_map.include?(name)
            directors_map[name] = Director.new(thisPerson)
        end
        
        
        for movie_descriptor in myJson["movies"]  # gather movies information
            name         = movie_descriptor["name"]
            runtime      = movie_descriptor["runtime"]
            categories   = Set.new( movie_descriptor["categories"] )
            release_date = Date.parse( movie_descriptor["release-date"]  )
        
            # Retrieve every Director and Actor object to place whithin current movie
            dirs = movie_descriptor["directors"].map { |nm| directors_map[nm] }
            acts = movie_descriptor["actors"].map { |nm| actors_map[nm] } 
        
            price      = movie_descriptor["price"]
            rent_price = movie_descriptor["rent_price"]
        
            premiere   = movie_descriptor["premiere"]
            discount   = movie_descriptor["discount"]
        
            this_movie = Movie.new(name,runtime,categories,release_date, SearchList.new(*dirs), SearchList.new(*acts),price,rent_price)
        
            # For every actor and director relate it to the current movie by making it star or director
            dirs.each { |dir| dir.directed << this_movie }  
            acts.each { |act| act.starred_in << this_movie }
        
            categories_set.merge(categories)  # update categories set with current categories
            
            # Update movie's price/rent_price depending on the presence of necessary attributes
            this_movie = Premiere.new(this_movie) if premiere
            this_movie = Discount.new(this_movie, discount) if discount != 0
        
            movie_catalog<<this_movie          # update movie catalog with current movie
        
        end
        
        
        # Client creation
        movies_db = Client.new(persons_map,actors_map,directors_map,movie_catalog,categories_set) 
        
        
        # Start of dialog
        # *Cree ask_input para pedir texto al usuario usando un prompt
        # *use puros case con numeros para manejar opciones que escoge el usuario
        # *movies_db lo es todo, todas las consultas se hacen sobre los objetos cargados en ella
        # *xD
        
        while (true) 
            puts "¿Qué desea hacer?\n"+
                "1) Crear nueva orden de alquiler\n"+
                "2) Crear nueva orden de compra\n"+
                "3) Mi usuario\n"+
                "4) Consultar catálogo\n"+
                "5) Salir\n"
        
            choice = ask_input # From here, jump to other functions
        
            case choice.to_i
            when 1 
            rent_order movies_db
            when 2 
            throw NotImplementedError
            when 3 
            check_user movies_db
            when 4 
            throw NotImplementedError
            when 5 
            puts "Hasta la proxima"
            break
            else
            inform "Opción inválida. Debe seleccionar un número del 1 al 7" 
            end
            
        end    
    end

    def rent_order movies_db
  
        while (true) 
      
            puts "Ingrese el nombre de la pelicula que desea alquilar" 
            req_movie = ask_input
            
            search_result = movies_db.movie_catalog.list.find { |movie| movie.name == req_movie }
            
            if search_result.nil? 
                while ( true ) 
                puts "La pelicula ingresada no existe. ¿Qué desea hacer?\n"+
                    "1) Regresar\n"+
                    "2) Consultar catálogo\n"+
                    "3) Repetir consulta\n"
                choice = ask_input
            
                case choice.to_i
                when 1
                    return 
                when 2
                    throw NotImplementedError # llama a la opcion 3 del menu principal
                    return
                when 3
                    break        
                else
                    puts "Ingrese una opcion valida" 
                end
            
                end
            else
                puts "Información de la película"
                puts search_result
                
                gd = true
                while ( gd ) 
        
                puts "Ingrese un metodo de pago. Disponibles:\n"+
                    "1) Dolares\n"+
                    "2) Bolivares\n"+
                    "3) Euros\n"+
                    "4) Bitcoins\n"+
                    "5) Volver a menu principal" 
        
                choice = ask_input
        
                in_dollars = "#{search_result.rent_price.dolars}"
        
                case choice.to_i
                when 1
                    puts "Total a pagar: "+in_dollars 
                when 2 
                    puts "Total a pagar: #{search_result.rent_price.dolars.in(:bolivar)} | "+in_dollars 
                when 3
                    puts "Total a pagar: #{search_result.rent_price.dolars.in(:euro)} | "+in_dollars 
                when 4
                    puts "Total a pagar: #{search_result.rent_price.dolars.in(:bitcoin)} | "+in_dollars 
                when 5
                    return 
                else
                    puts "Debe ingresar un metodo de pago valido"  
                    next
                end
        
                gd = false
                
                end
        
                puts "Desea continuar con la transaccion?\n1) Si\n2) No"
        
                choice = ask_input 
        
        
                case choice.to_i
                when 1 
                trans      = Transaction.new(search_result,:rent)  
                trans.date = Date.today + 2
        
                movies_db.user.transactions<<trans
                movies_db.user.rented_movies<<search_result
                puts "Su compra ha sido registrada"
                end
        
                return 
            end
        end  
    end
    def check_user movies_db

        if (movies_db.user.rented_movies.list.length > 0) 
            puts "peliculas alquiladas:" 
            print "\t"

            for mv in movies_db.user.rented_movies.list
                print mv.name
            end
            puts ""    
        end
    
        if (movies_db.user.owned_movies.list.length > 0) 
            puts "peliculas compradas:" 
            print "\t"
            for mv in movies_db.user.owned_movies.list
                print mv.name
            end
            puts ""    
        end
    
        puts "Ingrese el nombre de una películar a consultar. De no desearlo pulse enter: " 
    
        req_movie = ask_input.chomp
    
        if req_movie.empty? 
            return 
        else
            mv   = movies_db.user.owned_movies.list.find { |movie| movie.name == req_movie }
            mv_1 = movies_db.user.rented_movies.list.find { |movie| movie.name == req_movie }
            if mv.nil?  && mv_1.nil?
                puts "La pelicula a consultar no se encuentra entre sus peliculas rentadas o alquiladas"
            else
                puts "Informacion sobre #{req_movie}: "
                found_mv = mv.nil? ? mv_1 : mv
        
                puts found_mv 
                while ( true ) 
                puts "1) Consultar actores\n2) Consultar directores\n3) Regresar"
        
                choice = ask_input.chomp
        
                case choice.to_i
                when 1
                    puts "información sobre actores:" 
                    puts found_mv.actors
                when 2
                    puts "información sobre directores:"
                    puts found_mv.directors
                when 3
                    return
                else
                    puts "Ingrese una opcion valida" 
                end
        
                end
            end 
        end  
    end
end