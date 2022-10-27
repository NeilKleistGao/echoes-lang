# echoes-lang
[![Parsing Tests](https://github.com/NeilKleistGao/echoes-lang/actions/workflows/parsing_tests.yml/badge.svg?branch=main)](https://github.com/NeilKleistGao/echoes-lang/actions/workflows/parsing_tests.yml)

> typed lambda calculus + pattern matching macros == any language you like

A simple toy language.

## demos
### simple typed lambda calclus
```
lambda {
  x = false;
  not = (x: bool) => if (x) false else true;
  y = not(x);
}
```
