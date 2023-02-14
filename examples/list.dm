-- List: An 0-indexed dynamic array
-- Lists' meta table is stored in variable `List`

-- list may be indexed by negative number
l = [1, 2, 3]
assert$(l$[-1] == 3)

-- append to a list
l.append$(5)
assert$( l$[l.len$() - 1] == 5)

-- reverse a list
l.reverse$()
assert$( l$[0] == 5 )

-- insert an item to a specific location
-- panic if index out of bound
-- index may be negative number
l.insert$(0, 10)
assert$( l$[0] == 10 )
l.insert$(-1, -1)
assert$( l$[-2] == -1 )

-- remove an item from a specific location
-- panic is index out of bound
-- removed item is returned
item = l.remove$(0)
assert$( l$[0] == 5 )
assert$( item == 10 )

l.clear$()
assert$( l.len$() == 0 )

-- You can also call `List::<function name>$(l, ...)`
List::append$([], 1)
