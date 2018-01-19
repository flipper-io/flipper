pub mod c;

#[derive(Debug, Fail)]
pub enum GeneratorError {
    #[fail(display = "failed to generate C binding: {}", _0)]
    CRenderError(String),
}
