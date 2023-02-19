def fib n = 
    if n <= 1 then
        n
    else
        fib(n - 1) + fib(n - 2)
    end
end

assert(fib(15) == 610)
