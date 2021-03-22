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

class Person
end