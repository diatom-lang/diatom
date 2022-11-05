#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct DiatomParser;

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
