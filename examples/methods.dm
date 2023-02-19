a = {print = print}

-- call print as method
-- this will print `a` because a is implicitly
-- passed as the first parameter
a.print()

-- call print as static method
-- This will not print `a`
a::print('parameters here')

-- This will not work for tuple
a = (1, print)
-- `a` is not passed as parameter
a.1('parameters here')
