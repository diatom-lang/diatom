# Lexical Analysis Specification

## Numeric Literal

Numeric literals are a array of characters that begins with `'0'..'9'`. And will continue until reach any white space characters or ascii punctuations except for `'_'` and `'.'`. Specially, a `'+'` or `'-'` after an `'e'` or `'E'` is allowed for scientific notations.

Any `'_'` in the literal will be ignored when the literal is being parsed. This literal will be either parsed to a **64 bits signed integer** or a **64 bits float point** number. If the literal can not be parsed or cause an **overflow**, the Lexer will presents an error.

### Examples
```
946 9_123_444 0b111001 0o7763 0x8888 0X88_ff
0.123 125. 99e+3 88e-20 9E3
```
