# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`th-extras` is a small Hackage library: a grab bag of Template Haskell helpers whose main purpose is to hide
the ever-changing `template-haskell` AST behind a stable API that works across many GHC versions. The whole
library is a single module, `src/Language/Haskell/TH/Extras.hs`, which exports everything (no export list).

## Commands

```sh
cabal build                        # build the library (-Wall is on via the .cabal file)
cabal build -w ghc-9.6.7           # build against a specific GHC (ghcup-installed)
cabal check                        # package metadata check, as run in CI
cabal haddock                      # docs build, as run in CI
cabal sdist                        # CI builds from the sdist, so new files must be listed in the .cabal
```

There is no test suite; CI (`.github/workflows/haskell-ci.yml`) only verifies that the library builds,
passes `cabal check`, and builds haddocks on every GHC in `tested-with` (8.0.2 through 9.14.1).
For GHC >= 9.0 CI adds `-Werror=incomplete-patterns -Werror=incomplete-uni-patterns`, for >= 9.4
`-Werror=unused-packages`, and for >= 8.2 `-Werror=missing-methods -Werror=missing-fields`, so warnings that
are only warnings locally can fail CI.

## Multi-GHC compatibility

Compatibility is the core concern of every change:

- Version differences are handled with CPP, using both `__GLASGOW_HASKELL__` guards and
  `MIN_VERSION_template_haskell(...)`. Follow the surrounding style when adding cases for new TH
  constructors (e.g. new `Type` constructors in `substVarsWith`, which deliberately enumerates every
  constructor instead of using a catch-all so that new TH constructors get noticed).
- `th-abstraction`'s `Language.Haskell.TH.Datatype.TyVarBndr` is used to paper over the `TyVarBndr` flag
  changes (`TyVarBndr_`, `TyVarBndrSpec`, `TyVarBndrUnit`, `tvName`).
- When adding support for a new GHC: add it to `tested-with` in `th-extras.cabal`, bump the upper bounds on
  `template-haskell` / `th-abstraction` / `containers` as needed, then regenerate CI with `haskell-ci regenerate`
  (the workflow file is generated; don't hand-edit it).

## Releases

Never bump `version:` in `th-extras.cabal` and never modify `ChangeLog.md`; the maintainer handles releases.
