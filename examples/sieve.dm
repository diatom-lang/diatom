-- Example: Prime Number Sieve
def sieve n = 
    -- Create a list of [(0, true)], (1, true), ..]
    list = (0..).take(n)
        .map(fn x = (x, true))
        .collect()
   
    for i in 2..n//2 do
        if list[i].1 then
            (2..).map(fn x = x*i)
                .take_until(fn i = i >= n)
                .for_each(fn x = begin list[x].1 = false end)
        end
    end
    
    list.iter().skip(2)
        .filter(fn x = x.1)
        .map(fn x = x.0)
        .collect()
end
        
n = 100
println("All prime numbers within", n, "=")
println(sieve(n))
