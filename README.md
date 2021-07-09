# Modern-Compiler-Implementation-in-ML-book

https://www.cs.princeton.edu/~appel/modern/ml/

```sh
# Install the package manager
npm install -g esy

# Build dependencies
make install-deps

# Build all
make build

# Test all and exit
make test

# Test all and watch
make test-watch
```

## Tiger

Tiger is a simple but nontrivial language of the Algol family.

## Development

Generate messages from Menhir:

```sh
menhir --external-tokens Token --strict --explain --list-errors parser.mly > parser.messages
```
