f = 
    fn x = 
        if x > 5 then
            'x>5'
        elsif x > 4 then
            'x>4'
        else
            'x<=4'
        end

assert$(f$(0) == 'x<=4')
