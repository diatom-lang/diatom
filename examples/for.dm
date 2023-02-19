-- for loop
-- For loop is a macro to use iterators

-- The following program is equivalent

-- for x in iterator do
--      Body
-- end

-- iter = iterator.__iter()
-- loop
--     loop_sym = iter.__next()
--     if loop_sym is Option::None then
--         break
--     else
--         x = loop_sym.value
--         Body
--     end
-- end

-- For loop works for any instance of class `Iter`
-- For a value `x` to be used in for loop
-- x.__iter() must return an Iterator which has `__next` method
-- `__next` must return either `Option::None` or `Option::Some(<value>)`

-- List is an instance of class `Iter`
for x in [1, 'item2', {}] do
    print(x)
    print(' ')
end

-- Range is also an instance of class `Iter`
-- Range is a macro where `x..y` is equivalent to `Range(x, y)`
sum = 0
for x in 1..12 do
    sum = sum + x
end
println(sum)

