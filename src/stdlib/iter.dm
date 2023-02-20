-- Iterator meta table
Iter = {
    __iter = fn self = self
}

-- Tests if every element of the iterator matches a predicate.
def Iter.all self f =
    next = self.__next()
    until next is Option::None do
        if not f(next.value) then
            return false
        end
        next = self.__next()
    end
    true
end

-- Tests if any elements of the iterator matches a predicate.
def Iter.any self f =
    next = self.__next()
    until next is Option::None do
        if f(next.value) then
            return true
        end
        next = self.__next()
    end
    false
end

-- Collect all elements into a list
def Iter.collect self =
    list = []
    next = self.__next()
    until next is Option::None do
        list.append(next.value)
        next = self.__next()
    end
    list
end

-- count elements
def Iter.count self =
    n = 0
    next = self.__next()
    until next is Option::None do
        n = n + 1
        next = self.__next()
    end
    n
end

-- sum
def Iter.sum self =
    sum = 0
    next = self.__next()
    until next is Option::None do
        sum = sum + next.value
        next = self.__next()
    end
    sum
end

-- max, panic for empty list
def Iter.max self =
    next = self.__next()
    if next is Option::None then
        panic('Can not calculate maximum value for an empty iterator')
    end
    maximum = next.value
    until next is Option::None do
        if maximum < next.value then
            maximum = next.value
        end
        next = self.__next()
    end
    maximum
end

-- min, panic for empty list
def Iter.min self =
    next = self.__next()
    if next is Option::None then
        panic('Can not calculate minimum value for an empty iterator')
    end
    minimum = next.value
    until next is Option::None do
        if minimum > next.value then
            minimum = next.value
        end
        next = self.__next()
    end
    minimum
end

-- reduce
def Iter.reduce self f =
    next = self.__next()
    if next is Option::None then
        panic('Reduce an empty iterator!')
    end
    acc = next.value
    until next is Option::None do
        acc = f(acc, next.value)
        next = self.__next()
    end
    acc
end

-- for each
def Iter.for_each self f =
    next = self.__next()
    until next is Option::None do
        f(next.value)
        next = self.__next()
    end
end

-- fold
def Iter.fold self init f = 
    next = self.__next()
    until next is Option::None do
        init = f(init, next.value)
        next = self.__next()
    end
    init
end

-- map
def Iter.map self f = 
    map = {
        underlay = self,
    } <- Iter
    println(Option)
    def map.__next self =
        print(println)
        next = self.underlay.__next()
        if not(next is Option::None) then
            next.value = f(next.value)
        end
        next
    end
    map
end

-- filter
def Iter.filter self f = 
    filter = {
        underlay = self
    } <- Iter
    def filter.__next self = 
        loop 
            next = self.underlay.__next()
            if next is Option::None then
                return next
            elsif f(next.value) then
                return next
            end
        end
    end
    filter
end

-- skip
def Iter.skip self n = 
    ret = Option::Some(())
    skip = {
        underlay = self,
    } <- Iter
    def skip.__next self =
        skipped = 0
        next = self.underlay.__next()
        until skipped >= n or next is Option::None do
            next = self.underlay.__next()
            skipped = skipped + 1
        end
        self.__next = 
            fn self = self.underlay.__next()
        next
    end
    skip
end

-- take 
def Iter.take self n =
    take = {
        underlay = self,
        remain = n
    } <- Iter
    def take.__next self =
        if self.remain == 0 then
            Option::None
        else 
            next = self.underlay.__next()
            self.remain = self.remain - 1
            next
        end
    end
    take
end

-- zip
def Iter.zip self iter =
    zip = {
        iter1 = self,
        iter2 = iter
    } <- Iter
    def zip.__next self =
        next1 = self.iter1.__next()
        next2 = self.iter2.__next()
        if not( next1 is Option::None or next2 is Option::None ) then
            next1.value = (next1.value, next2.value)
            next1
        else
            Option::None
        end
    end
    zip
end

-- step_by
def Iter.step_by self n =
    n = n - 1
    step_by = {
        underlay = self
    } <- Iter
    -- skip by step then
    def step_next self =
        skipped = 0
        next = self.underlay.__next()
        until skipped >= n or next is Option::None do
            skipped = skipped + 1
            next = self.underlay.__next()
        end
        next
    end
    def step_by.__next self =
        -- always return first item
        next = self.underlay.__next()
        self.__next = step_next
        next
    end
    step_by
end

-- enum
def Iter.enum self =
    enum = {
        underlay = self,
        count = 0
    } <- Iter
    def enum.__next self =
        next = self.underlay.__next()
        if next is Option::None then
            next
        else 
            next.value = (self.count, next.value)
            self.count = self.count + 1
            next
        end
    end
    enum
end

-- take_until
def Iter.take_until self f =
    take_until = {
        underlay = self,
    } <- Iter
    def take_until.__next self =
        next = self.underlay.__next()
        if next is Option::None then
            next
        elsif f(next.value) then
            Option::None
        else
            next
        end
    end
    take_until
end
