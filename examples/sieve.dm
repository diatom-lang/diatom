-- Example: Prime Number Sieve
import sqrt from std.math
import {DateTime, Duration} from std.os.time

def sieve n = 
    -- Create a list of [(0, true)], (1, true), ..]
    list = (0..).take(n)
        .map(fn x = (x, true))
        .collect()
   
    for i in 2..sqrt(n).ceil().int() do
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

start = DateTime::now()
sieve_n = sieve(n)
finish = DateTime::now()

println("Finished in", Duration::new(start, finish).show())
println("All prime numbers within", n, "=") 
println(sieve_n)
