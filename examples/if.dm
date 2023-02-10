f = 
    fn x = 
        if x > 5 then
            print$('>5')
        elsif x > 4 then
            print$('>4')
        else
            print$('<=4')
        end

f$(0)
