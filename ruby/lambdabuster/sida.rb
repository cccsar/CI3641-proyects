require 'json' 

def getParsedJson filename

  if (File.exist?(filename)) then
     fObject = File.open(filename) 
     fString = File.read(fObject) 
     return JSON.parse(fString)
  end

end


# asume json bien formateado
def actorsDirectorsTraversal parsedJson 

  topLevel = ["directors","actors"]
  attributes = ["name","birthday","nationality"] 

  for kind in topLevel
   for subjects in parsedJson[kind] 
       for at in attributes
         print "#{subjects[at]} " 
       end
       print "\n" 
   end
  end

end


# asume json bien formateado
def moviesTraversal parsedJson

  atts = ["name","runtime","categories","release-date","actores","price","rent-price","premiere","discount"]

  for movie in parsedJson["movies"] 
    for att in atts
        print "#{movie[att]} " 
      print "\n" 
    end
  end

end
