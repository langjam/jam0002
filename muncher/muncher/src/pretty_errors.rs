use std::{ops::Range, path::Path};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    files::SimpleFiles,
    term::{termcolor::{WriteColor}, Config, Chars},
};

use crate::{Error, Span};

pub(crate) fn emit(
    error: &Error,
    source_name: &Path,
    source: &str,
    output: &mut dyn WriteColor,
) {
    let mut files = SimpleFiles::new();
    let file = files.add(source_name.display().to_string(), source);

    let mut labels = Vec::new();
    labels.push(Label {
        style: LabelStyle::Primary,
        file_id: file,
        range: convert_span(error.span),
        message: error.msg.clone(),
    });
    for note in &error.notes {
        labels.push(Label {
            style: LabelStyle::Secondary,
            file_id: file,
            range: convert_span(note.span),
            message: note.msg.clone(),
        });
    }

    let diagnostic = Diagnostic {
        severity: codespan_reporting::diagnostic::Severity::Error,
        code: None,
        message: error.msg.clone(),
        labels,
        notes: Vec::new(),
    };

    codespan_reporting::term::emit(
        output,
        &Config {
            display_style: codespan_reporting::term::DisplayStyle::Rich,
            chars: Chars::ascii(),
            .. Config::default()
        },
        &files,
        &diagnostic,
    ).expect("diagnostic emit failed");
}

fn convert_span(span: Span) -> Range<usize> {
    span.start.offset..span.end.offset
}
