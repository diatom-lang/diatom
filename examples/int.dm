-- Int type (64-bit signed integer)
-- Meta table is stored in `Int`

i = -10
assert( i.abs() == 10 )
assert( i.float() < 0 )

-- Int also provies max and min 
-- possible value for this type
imax = Int::MAX
imin = Int::MIN
