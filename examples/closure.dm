x1 = ()
x2 = ()
f = fn = begin
    a = 1
    x1 = fn = a
    x2 = fn x = begin 
        a = x
    end
end
f$()
x2$(10)
x1$() == 10
