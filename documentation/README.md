# Flipper Documentation

> ⚠️ **Warning:** [Rust] must be installed prior to building the documentation.

This documentation uses [mdBook] as the static site generator. To install it:

[Rust]: https://www.rust-lang.org/en-US/
[mdBook]: https://github.com/azerupi/mdBook

```
$ cargo install mdbook
```

## Building

To build the documentation:

```
$ mdbook build
```

## Live Testing

To test the documentation:

```
$ mdbook serve
```

This will serve the documentation live on `localhost:3000`.