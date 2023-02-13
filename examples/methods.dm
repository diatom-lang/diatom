a = {print = print}

-- call print as method
a.print$('parameters here')

-- call print as static method
a::print$('parameters here')

-- This will not work for tuple
a = (1, print)
a.1$('parameters here')
