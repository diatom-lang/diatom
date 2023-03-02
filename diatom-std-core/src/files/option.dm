-- Option Monad
Option = {
    Some = 
        fn x = {value = x} <- Option,
    None = {} <- Option,
}

Some = Option::Some
None = Option::None
