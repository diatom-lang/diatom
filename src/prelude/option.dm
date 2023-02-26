-- Option Monad
Option = {
    Some = 
        fn x = {value = x} <- Option,
    None = {} <- Option,
}

