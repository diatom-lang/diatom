meta_table = {
    positive = fn self i = i > 0,
    name = 'meta table'
}

table = {key = 'key'} <- meta_table

assert(table.name == 'meta table')
assert(table.key == 'key')
assert(table.positive(10))
