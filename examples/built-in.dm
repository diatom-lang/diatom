-- These following functions are built in method

-- Print anything (Safe for reference cycle)
-- takes any amount of parameters
cycle = {ref = cycle}
print$(cycle)
print$()
print$(1, 2, 3)

-- Assert a bool value
-- panic if `false` is passed
-- Also panic if more than one parameters are passed
-- or pass a non-bool is passed
assert$(true)

-- Immediately trigger a panic
-- takes a single string or no parameters
panic

-- Mark an unreachable part of code
-- immediately panic when get executed
-- take no parameters
unreachable
