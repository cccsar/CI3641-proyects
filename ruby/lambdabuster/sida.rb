require 'json' 

def getParsedJson filename

  if (File.exist?(filename)) then
     fObject = File.open(filename) 
     fString = File.read(fObject) 
     return JSON.parse(fString)
  end

end
