meta_table = {
    print = (fn self = print$(self, 'meta print') ),
    name = 'meta table'
}

table = {key = 'table key'} <- meta_table

table.print$()
print$(table.name)
