use pest::{error::Error, iterators::Pairs, pratt_parser::PrattParser};

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
struct RawDiatomParser;

/// # Main Parser for the Diatom language
/// Take a mutable str as input and return an AST for mid-end.
/// # Examples
/// ```
/// use diatom::parser::DiatomParser;
/// 
/// let mut program = r#"
/// a = 5
/// b = [a, 6, 's'].len()
/// "#.to_string();
///
/// let result = DiatomParser::parse_main(&mut program);
/// match result {
///     Ok(ast) => println!("{:?}", ast),
///     Err(err) => println!("{:?}", err)
/// };
/// ```
pub struct DiatomParser {}

impl pest::Parser<Rule> for DiatomParser {
    /// Parse raw input without preprocessing.  
    /// Note that this will **disable** the use of **newline** in parentheses!
    fn parse<'i>(rule: Rule, input: &'i str) -> Result<Pairs<'i, Rule>, Error<Rule>> {
        RawDiatomParser::parse(rule, input)
    }
}

impl DiatomParser {
    fn remove_unnecessary_newline(input: &mut str) {
        unimplemented!()
    }

    /// Parse a single grammar rule.  
    /// Automatically remove **newline** in parentheses, since these are redundant and can confuse
    /// the parser.
    pub fn parse_processed<'i>(
        rule: Rule,
        input: &'i mut str,
    ) -> Result<Pairs<'i, Rule>, Error<Rule>> {
        Self::remove_unnecessary_newline(input);
        <RawDiatomParser as pest::Parser<Rule>>::parse(rule, input)
    }

    /// Parse the whole programme.  
    /// Automatically remove **newline** in parentheses, since these are reduntant and can confuse
    /// the parser.
    pub fn parse_main<'i>(input: &'i mut str) -> Result<Pairs<'i, Rule>, Error<Rule>> {
        Self::parse_processed(Rule::main, input)
    }
}
struct ExprParser {
    parser: PrattParser<Rule>,
}

impl ExprParser {
    fn new() -> Self {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        let parser = PrattParser::new()
            .op(Op::infix(op_assign, Right))
            .op(Op::postfix(op_condition))
            .op(Op::infix(op_range, Left))
            .op(Op::infix(op_or, Left))
            .op(Op::infix(op_and, Left))
            .op(Op::infix(op_eq, Left)
                | Op::infix(op_ne, Left)
                | Op::infix(op_le, Left)
                | Op::infix(op_lt, Left)
                | Op::infix(op_ge, Left)
                | Op::infix(op_gt, Left))
            .op(Op::infix(op_plus, Left) | Op::infix(op_minus, Left))
            .op(Op::infix(op_mul, Left)
                | Op::infix(op_div, Left)
                | Op::infix(op_mod, Left)
                | Op::infix(op_div_floor, Left))
            .op(Op::prefix(op_plus) | Op::prefix(op_minus) | Op::prefix(op_not))
            .op(Op::postfix(op_index)
                | Op::postfix(op_call)
                | Op::postfix(op_chain)
                | Op::postfix(op_chain));
        ExprParser { parser }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    fn test_rule_ok(rule: Rule, input: &str) {
        let result = DiatomParser::parse(rule, input);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.as_str(), input);
    }

    fn test_rule_err(rule: Rule, input: &str) {
        let result = DiatomParser::parse(rule, input);
        if !result.is_ok() {
            return;
        };
        let result = result.unwrap();
        assert_ne!(result.as_str(), input);
    }

    #[test]
    fn test_integer_parse() {
        test_rule_ok(Rule::int, "0b00_1__0_");
        test_rule_ok(Rule::int, "1290__");
        test_rule_ok(Rule::int, "0xF0ac_1__0_");
        test_rule_ok(Rule::int, "0B00_1__0_");
        test_rule_ok(Rule::int, "0o007771_0_");

        test_rule_err(Rule::int, "0O89");
    }
}
