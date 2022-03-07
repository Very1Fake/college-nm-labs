<h1 align="center">Numeric Methods Labs</h1>

<div align="center">

[![Project license](https://img.shields.io/github/license/Very1Fake/college-nm-labs)](./LICENSE)

</div>

## Usage

### Testing

For running tests you to install `cargo-nextest`:

```bash
$ cargo install cargo-nextest
```

Then you can easily run tests (only `nm-math` crate has unittests):

```bash
$ cargo nextest run -p nm-math
```

### Bench

For performing benchmark tests you need to install `cargo-criterion`:

```bash
$ cargo install cargo-criterion
```

To run benches simply type (only `nm-math` crate has bench tests):

```bash
$ cargo criterion -p nm-math
```

You can find criterion reports in `./target/criterion/reports` directory

## License
Licensed under the GNU GPL 3.0. Copyright 2022 Timur Israpilov. [Copy of the license](./LICENSE).