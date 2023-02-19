-- Float type (64-bit float point number)
-- Meta table stored in `Float`

-- Float can not be compared for equality
-- `1.23 == 1.22` will cause diatom panic
-- Instead, compare for inequality
assert((1.23 - 1.23).abs() < 1e-10)

f = 1.2235
assert(f.floor().int() == 1)
assert(f.ceil().int() == 2)
assert((-1.0).abs().int() == 1)

-- positive and negative inf
inf = Float::INF
assert( inf.is_inf() )

neg_inf = Float::NEG_INF
assert( neg_inf.is_inf() )

-- A nan value, the value itself depends on rust version
nan = Float::NAN
assert( nan.is_nan() )

-- Float also provides MAX and MIN
fmax = Float::MAX
fmin = Float::MIN
