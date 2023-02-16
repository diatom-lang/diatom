-- Option Monad
Option = {
    Some = (
        fn x = {value = x} <- Option
    ),
    None = {} <- Option
}

-- Iterator meta table
Iter = {
    __iter = fn self = self
}

-- Iterator for list
List.__iter = 
    fn self = {
        list = self,
        idx = 0,
        -- Avoid create a new table every time
        _ret = Option::Some$(()),
        __next =
            fn self = 
                if self.idx >= self.list.len$() then
                    Option::None
                else
                    idx = self.idx
                    self.idx = idx + 1
                    self._ret.value = self.list$[idx]
                    self._ret
                end
    } <- Iter

List.iter = List.__iter

-- Range (x..end) Constructor
Range =
    fn x y = {
        x = x,
        y = y,
        _ret = Option::Some$(()),
        __iter = fn self = self,
        __next = 
            fn self = 
                if self.x < self.y then
                    self._ret.value = self.x
                    self.x = self.x + 1
                    self._ret
                else
                    Option::None
                end
    } <- Iter
    
