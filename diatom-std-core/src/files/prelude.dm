import {
    print, 
    println, 
    panic, 
    assert, 
    pause, 
    resume, 
    collect,
} from prelude.built_in

unreachable = 
    fn = panic("Unreachable code reached")
todo = 
    fn = panic("Not implemented yet")

Gc = {
    collect = collect,
    pause = pause,
    resume = resume,
}

-- Initialize `Int`
begin
    import {abs, float, MAX, MIN} from prelude.int
    Int.abs = abs
    Int.float = float
    Int.MAX = MAX()
    Int.MIN = MIN()
end

-- Initialize `Float`
begin
    import {
        MAX,
        MIN,
        INF,
        NEG_INF,
        NAN,
        abs,
        floor,
        ceil,
        int,
        round,
        is_nan,
        is_inf,
    } from prelude.float
    Float.MAX = MAX()
    Float.MIN = MIN()
    Float.INF = INF()
    Float.NEG_INF = NEG_INF()
    Float.NAN = NAN()
    Float.abs = abs
    Float.floor = floor
    Float.ceil = ceil
    Float.int = int
    Float.round = round
    Float.is_nan = is_nan
    Float.is_inf = is_inf
end
