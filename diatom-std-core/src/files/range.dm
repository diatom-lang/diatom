-- Range (start..end) Constructor
def Range start limit = 
    {
        start = start,
        limit = limit,
        _ret = Some(()),
        __iter = fn self = self,
        __next = 
            fn self = 
                if self.start < self.limit then
                    self._ret.value = self.start
                    self.start = self.start + 1
                    self._ret
                else
                    None
                end
    } <- Iter
end
    
