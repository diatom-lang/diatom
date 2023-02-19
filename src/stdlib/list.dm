--- Iterator for list
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
