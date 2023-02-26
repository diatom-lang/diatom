use std::borrow::Cow;

use crossterm::style::Color;
use reedline::Prompt;

#[derive(Default)]
pub struct DiatomPrompt {}

impl Prompt for DiatomPrompt {
    fn get_prompt_color(&self) -> Color {
        Color::DarkGrey
    }
    fn render_prompt_left(&self) -> Cow<str> {
        Cow::Borrowed("diatom > ")
    }

    fn render_prompt_right(&self) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _prompt_mode: reedline::PromptEditMode) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        Cow::Borrowed("   > ... ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reedline::PromptHistorySearch,
    ) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn get_indicator_color(&self) -> Color {
        Color::Reset
    }

    fn get_prompt_right_color(&self) -> Color {
        Color::Reset
    }

    fn right_prompt_on_last_line(&self) -> bool {
        false
    }
}
