-- Example for operating garbage collector

a = {}
-- Pause garbage collect
Gc::pause()
a = [1, 2, 3,]

-- Resume garbage collect 
Gc::resume()
-- Immediately collect garbage
Gc::collect()

