class SearchList
    def initialize(*elems)
        @list = []
        for elem in elems
            @list.append(elem)
        end
        for i in @list
            puts @list[0]
        end
    end

    def <<(elem)
        @list<< elem
        self
    end

    def to_s
    
    end
    def +(other )
    
    end

    def each (* args , &block )
        @list.each(* args , &block )
    end
end

class Person
end