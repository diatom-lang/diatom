a = 0
until a >= 100 do
    a = a + 1
end
assert(a == 100)

a = 0
loop
    if a < 100 then
        a = a + 1
        continue
    else
        break
    end
end
assert(a == 100)

