use pest::pratt_parser::PrattParser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct DiatomParser;

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
