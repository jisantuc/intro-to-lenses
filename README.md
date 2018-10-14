Intro to Lenses
===============

This repository contains [`patat`-able](https://github.com/jaspervdj/patat) slides on lenses,
with example code in scala, haskell, and javascript.

To view the presentation, install `patat`, then `patat lenses.md`.

Example code is designed to be run in specific environments:

- `stack ghci --ghc-options -package=lens` for the Haskell examples
- an [`ammonite`](http://ammonite.io/) repl with [`monocle`](https://github.com/julien-truffaut/Monocle) availble for the scala examples:

  - install ammonite
  - From the ammonite repl:

```scala
import $ivy.`com.github.julien-truffaut::monocle-core:1.5.0`
import $ivy.`com.github.julien-truffaut::monocle-macro:1.5.0`
```

- a nodejs repl with [`ramda`](https://ramdajs.com/) available for javascript

Once you have the appropriate repl available, everything should Just Work :tm: with copy-pasta
or with magic file imports, depending on your setting.
