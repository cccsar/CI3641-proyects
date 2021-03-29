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

# get an option and validate it
def select_option options
  while true
    puts "¿Qué deseas hacer"
    i = 1
    for option in options
      puts "\t#{i}) #{option}"
      i += 1
    end

    input = ask_input.to_i

    if input >= 1 &&  input <= options.length
      return input
    else
      inform "'#{input}' no es una opción válida"
    end
  end
end

# Get an input, requesting it by 'request' string and validate it with a given block
def request_input (request, &validation)
  while true
    puts request
    input = ask_input.chomp

    if validation.call(input)
      return input
    else
      inform "'#{input}' no es una opción válida"
    end
  end
end

# -- Main Code ------------------- +
class Client
    attr_reader  :persons_map, :actors_map, :directors_map, :movie_catalog, :categories_set, :user
    
    def initialize (persons_map    = Hash.new(), 
                    actors_map     = Hash.new(),
                    directors_map  = Hash.new(),
                    movie_catalog  = SearchList.new(),
                    categories_set = Set[],
                    user           = User.new()
                   ) 
      @persons_map    = persons_map    # map of person by name 
      @actors_map     = actors_map     # map of actors by name, coungrent with known persons
      @directors_map  = directors_map  # map of directors by name, coungrent with known persons
      @movie_catalog  = movie_catalog  # SearchList of movies
      @categories_set = categories_set # set of movie categories
      @user           = user           # a user
    end

    def main 
        puts "Bienvenido a Lambdabuster!"
        my_json = read_json_file
        gather_json_info my_json
        interaction
    end

    def read_json_file # Proper JSON read.
        const = "Ingrese el nombre de un archivo JSON correctamente formateado: "

        filename = request_input(const) { |fname| File.exist?(fname) }
    
        fObject = File.open(filename) 
        fString = fObject.read 
        fObject.close 
    
        return JSON.parse(fString)

    end 
        
    def gather_json_info my_json
        
        # Traversal of json and information gathering.
        
        for actor_descriptor in my_json["actors"]  # Gather actors information
            name        = actor_descriptor["name"]
            birthday    = Date.parse ( actor_descriptor["birthday"]  )
            nationality = actor_descriptor["nationality"]
        
            thisPerson = Person.new(name, birthday, nationality)  
        
            @persons_map[name] = thisPerson 
            @actors_map[name]  = Actor.new(thisPerson) 
        end
        
        for director_descriptor in my_json["directors"] # Gather directors information
            name        = director_descriptor["name"]
            birthday    = Date.parse( director_descriptor["birthday"]  )
            nationality = director_descriptor["nationality"]
        
            thisPerson = Person.new(name, birthday, nationality) 
        
            @persons_map[name]   = thisPerson if !@persons_map.include?(name)
            @directors_map[name] = Director.new(thisPerson)
        end
        
        
        for movie_descriptor in my_json["movies"]  # gather movies information
            name         = movie_descriptor["name"]
            runtime      = movie_descriptor["runtime"]
            categories   = Set.new( movie_descriptor["categories"] )
            release_date = Date.parse( movie_descriptor["release-date"]  )
        
            # Retrieve every Director and Actor object to place whithin current movie
            dirs = movie_descriptor["directors"].map { |nm| @directors_map[nm] }
            acts = movie_descriptor["actors"].map { |nm| @actors_map[nm] } 
        
            price      = movie_descriptor["price"]
            rent_price = movie_descriptor["rent_price"]
        
            premiere   = movie_descriptor["premiere"]
            discount   = movie_descriptor["discount"]
        
            this_movie = Movie.new(name,runtime,categories,release_date, SearchList.new(*dirs), SearchList.new(*acts),price,rent_price)
        
            # For every actor and director relate it to the current movie by making it star or director
            dirs.each { |dir| dir.directed << this_movie }  
            acts.each { |act| act.starred_in << this_movie }
        
            @categories_set.merge(categories)  # update categories set with current categories
            
            # Update movie's price/rent_price depending on the presence of necessary attributes
            this_movie = Premiere.new(this_movie) if premiere
            this_movie = Discount.new(this_movie, discount) if discount != 0
        
            @movie_catalog<<this_movie          # update movie catalog with current movie
        
        end
        
    end

    def interaction    
        
      interaction_options = ["Crear nueva orden de alquiler","Crea nueva orden de compra","Mi usuario", "Consultar catálogo","Salir"]
      choice = select_option interaction_options
      
      case choice
      when 1 
        an_order true
      when 2 
        an_order false
      when 3 
        check_user 
      when 4 
        filtering
      when 5 
        puts "Hasta la proxima"
        return
      else
        inform "Opción inválida. Debe seleccionar un número del 1 al 7" 
      end

      interaction
    end

    def an_order type
  
        payment_meth = type ? "alquilar" : "comprar"
        puts "Ingrese el nombre de la pelicula que desea #{payment_meth}" 
        req_movie = ask_input
        
        search_result = @movie_catalog.list.find { |movie| movie.name == req_movie }
        
        if search_result.nil? 
            out_options = ["Regresar","Consultar catálogo","Repetir consulta"]

            puts "La pelicula ingresada no existe\n"

            choice = select_option out_options
         
            case choice
            when 1
              return 
            when 2
              filtering 
            when 3
              an_order type
            else
              puts "Ingrese una opcion valida" 
            end
          
        else
         puts "Información de la película"
         puts search_result
           
          gd = true
          
          while ( gd ) 
    
            puts "Ingrese un metodo de pago. Disponibles:\n"+
                "\t1) Dolares\n"+
                "\t2) Bolivares\n"+
                "\t3) Euros\n"+
                "\t4) Bitcoins\n"+
                "\t5) Volver a menu principal" 
    
            choice = ask_input
            actual_price = type ? search_result.rent_price.dolars : search_result.price.dolars
            in_dollars = "#{actual_price}"
    
            case choice.to_i
            when 1
                puts "Total a pagar: "+in_dollars 
            when 2 
                puts "Total a pagar: #{actual_price.in(:bolivar)} | "+in_dollars 
            when 3
                puts "Total a pagar: #{actual_price.in(:euro)} | "+in_dollars 
            when 4
                puts "Total a pagar: #{actual_price.in(:bitcoin)} | "+in_dollars 
            when 5
                return 
            else
                puts "Debe ingresar un metodo de pago valido"  
                next
            end
    
            gd = false
            
          end
    
          options = ["Si","No"] 

          puts "Desea continuar con la transaccion?"
   
          choice = select_option options 
  
          if choice == 1
            trans      =  Transaction.new(search_result, type ? :rent : :buy)
            trans.date = Date.today + 2 if type 
  
            @user.transactions<<trans
            
            if type 
              @user.rented_movies<<search_result
            else
              @user.owned_movies<<search_result
            end

            puts "Su compra ha sido registrada"
          end

          return 
        end
    end

    def check_user 

        if (@user.rented_movies.list.length > 0) 
            puts "peliculas alquiladas:" 
            print "\t"

            for mv in @user.rented_movies.list
                print mv.name
            end
            puts ""    
        end
    
        if (@user.owned_movies.list.length > 0) 
            puts "peliculas compradas:" 
            print "\t"
            for mv in @user.owned_movies.list
                print mv.name
            end
            puts ""    
        end
    
        while ( true ) 
          puts "Ingrese el nombre de una películar a consultar. De no desearlo pulse enter: " 
      
          req_movie = ask_input.chomp
      
          if req_movie.empty? 
              return 
          else
            mv   = @user.owned_movies.list.find { |movie| movie.name == req_movie }
            mv_1 = @user.rented_movies.list.find { |movie| movie.name == req_movie }

            if mv.nil?  && mv_1.nil?
              puts "La pelicula a consultar no se encuentra entre sus peliculas rentadas o alquiladas"
            else
              puts "Informacion sobre #{req_movie}: "
              found_mv = mv.nil? ? mv_1 : mv
        
              puts found_mv 
  
              while ( true ) 
                consult_opt = ["Consultar actores", "Consultar directores", "Regresar"] 
    
                choice = select_option consult_opt
        
                case choice
                when 1
                    puts "información sobre actores:" 
                    found_mv.actors.list.each { |act| puts "\t #{act}" } 
                when 2
                    puts "información sobre directores:"
                    found_mv.directors.list.each{ |act| puts "\t #{act}" } 
                when 3
                    break 
                end
              end
  
            end 
          end  
       end
    end

    def filtering


      while ( true ) 
        puts "Escoga una de las siguientes opciones:"

        puts "\t1) Mostrar todas\n"+
             "\t2) Filtrar\n" 
  
        case ask_input.chomp.to_i
        when 1
          for movie in @movie_catalog
            puts movie
          end

          break 

        when 2

          filtered_movies = Set.new() # Set that maintains filtered movies at the moment

          while ( true ) 

            puts "Seleccione alguno de los siguientes filtros:"
            puts "\t1) Nombre\n"+
                 "\t2) Año\n"+ 
                 "\t3) Nombre de director\n"+ 
                 "\t4) Nombre de actor\n"+
                 "\t5) Duración\n"+
                 "\t6) Categorías\n"+
                 "\t7) Precio de compra\n"+
                 "\t8) Precio de alquiler\n"
    
            case ask_input.chomp.to_i
            when 1
              filtered_movies = name_match(filtered_movies,:name) 
            when 2
              filtered_movies = manage_ord(filtered_movies,"entero",:release_date)  
            when 3
              filtered_movies = name_match(filtered_movies,:directors) 
            when 4
              filtered_movies = name_match(filtered_movies,:actors)
            when 5
              filtered_movies = manage_ord(filtered_movies,"entero",:runtime) 
            when 6
              accum_cathegories = Set.new() 

              finished = false
              while ( true ) 
                puts "Seleccione alguna de las siguientes categorias: " 

                for cat in categories_set
                  puts cat
                end

                req_cathegory = ask_input.chomp

                if categories_set.include?( req_cathegory )  
                  accum_cathegories.add( req_cathegory ) 

                  while ( true ) 
                    puts "¿Desea añadir más filtros?"
                    puts "\t1) Si\n\t2) No\n"

                    case ask_input.chomp.to_i
                    when 1
                      break  
                    when 2 
                      for cat in accum_cathegories
                        matching_movies = movie_catalog.list.filter { |mv| mv.categories.include?( cat ) } 
                        filtered_movies = filtered_movies | matching_movies
                      end

                      finished = true
                      break
                    else
                      puts "Debe ingresar una opción válida"
                    end
                  end

                  break if finished

                else
                  puts "La categoria seleccionada no corresponde a ninguna de las existentes en sistema." 
                end


              end

               
            when 7
              filtered_movies = manage_ord(filtered_movies,"en punto flotante",:price) 
            when 8
              filtered_movies = manage_ord(filtered_movies,"en punto flotante",:rent_price) 
            else
              puts "Debe ingresar una opcion valida"
            end

            puts "¿Qué desea hacer ahora?"
            puts "\t1) Aplicar otro filtro\n"+
                 "\t2) Buscar\n"

            while (true ) 
              
              case ask_input.chomp.to_i
              when 1
                break
              when 2
                for movie in filtered_movies
                  puts movie
                end

                return 
              else
                puts "Debe ingresar una opcion valida"
              end

            end
  
          end
  
        else
          puts "Debe ingresar una opcion valida"
        end

      end


    end

    def manage_ord (filtered_movies,type,atom)  

      puts "Ingrese un numero #{type} " 
      num = ask_input.chomp.to_i

      to_add = []  

      while ( true ) 
        puts "Escoja una de las siguientes opciones:"
        puts "\t1) < num\n"+
             "\t2) <= num\n"+
             "\t3) == num\n"+
             "\t4) > num\n"+
             "\t5) >= num\n"

        case ask_input.chomp.to_i
        when 1
          to_add = @movie_catalog.list.filter do 
            |movie| 

            val_tc = movie.method(atom).()

            val_tc.class == Date ? val_tc.year < num : val_tc < num 
          end
        when 2
          to_add = @movie_catalog.list.filter do 
            |movie| 

            val_tc = movie.method(atom).()

            val_tc.class == Date ? val_tc.year <= num : val_tc <= num 
          end
        when 3
          to_add = @movie_catalog.list.filter do 
            |movie| 

            val_tc = movie.method(atom).()

            val_tc.class == Date ? val_tc.year == num : val_tc == num 
          end
        when 4
          to_add = @movie_catalog.list.filter do 
            |movie| 

            val_tc = movie.method(atom).()

            val_tc.class == Date ? val_tc.year > num : val_tc > num 
          end
        when 5
          to_add = @movie_catalog.list.filter do 
            |movie| 

            val_tc = movie.method(atom).()

            val_tc.class == Date ? val_tc.year >= num : val_tc >= num 
          end
        else
          puts "Debe ingresar una opcion valida" 
          next
        end

        filtered_movies = filtered_movies | to_add.to_set
        return filtered_movies

      end

    end

    def name_match (filtered_movies, atom) 

      to_add = [] 

      options = ["Coincidencia exacta", "Coincidencia parcial"] 
      choice = select_option options

      puts "Ingrese el string a matchear:" 
      to_match = ask_input


      case choice
      when 1
        to_add = @movie_catalog.list.filter do 
         | movie |
         chk = movie.method(atom).()
          
         if chk.class == String
           chk == to_match
         else
           chk.list.map { |aod| aod.name }.include?(to_match) 
         end

        end
      when 2
       to_add = @movie_catalog.list.filter do 
         |movie| 
         chk = movie.method(atom).()

         if chk.class == String
           chk.include?(to_match) 
         else
           chk.list.map { |aod| aod.name }.filter{ |aod| aod.include?(to_match) }.any?
         end

       end
      end

      filtered_movies = filtered_movies | to_add.to_set
      return filtered_movies
    end

end

client = Client.new() 

client.main
