max = (1..20)
        .map(fn x = x*3)
        .max()
assert(max == 19*3)

-- all odd number within 100
max_odd = (0..100)
            .filter(fn x = not(x%2 == 0))
            .collect()
            [-1]
assert(max_odd == 99)

-- Use an infinite list
l = (1..)
    .filter(fn x = x%13 == 0)
    .filter(fn x = x%12 == 0)
    .take(5)
    .collect()
assert(l[-1] == 780)

-- TODO: More examples


