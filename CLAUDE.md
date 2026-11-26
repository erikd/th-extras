# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`th-extras` is a small Hackage library: a grab bag of Template Haskell helpers whose main purpose is to hide
the ever-changing `template-haskell` AST behind a stable API that works across many GHC versions. The whole
library is a single module, `src/Language/Haskell/TH/Extras.hs`, which exports everything (no export list).

## Commands

```sh
cabal build                        # build the library (-Wall is on via the .cabal file)
cabal test                         # run the Hedgehog property tests
cabal test -w ghc-X.Y.Z            # build and test against a specific GHC (ghcup-installed)
cabal check                        # package metadata check, as run in CI
cabal haddock                      # docs build, as run in CI
cabal sdist                        # CI builds from the sdist, so new files must be listed in the .cabal
```

A failing property prints a `recheckAt (Seed ...) "..." prop_name` line; run it in `cabal repl test` to replay
just that failure.

CI (`.github/workflows/haskell-ci.yml`) builds the library and tests, runs `cabal check` and builds haddocks on
every GHC in `tested-with`. On newer GHCs it also adds `-Werror` for incomplete patterns, unused packages and
missing methods/fields (applied to the test suite too), so warnings that are only warnings locally can fail CI.

## Tests

The test suite (`test/`) is Hedgehog properties that compare library functions against independent oracles:

- `test/Gen.hs` holds generators for TH syntax (`Type`, `Pat`, `Dec`, `Con`, kinds, and inputs for
  `substVarsWith`) that cover every constructor available in the `template-haskell` being built against
  (CPP-guarded with `MIN_VERSION_template_haskell`), plus reference implementations (e.g. `freeVars`,
  `patVars`, `decNames`, `typeHead`) used as oracles. Several generators deliberately write the same thing in
  different but equivalent syntax (parentheses, kind signatures, infix forms) to check that results don't
  depend on it.
- `test/Fixtures.hs` holds declarations that are reified at compile time, for functions that need `reify`
  (e.g. `tyConArity`), and splice helpers that run the function at compile time and splice in the result.
  Fixtures needing newer extensions are CPP-guarded on `__GLASGOW_HASKELL__`. GHC's stage restriction means a
  splice can only run functions defined in another module, which is why the helpers live here and the
  splices in `Main`.
- `test/Main.hs` holds the properties, registered explicitly in a `Group` (no TH `discover`).
- Library functions report failure with `error`, so tests must force results where Hedgehog can catch the
  exception (e.g. `eval`, or `try`/`evaluate` with `evalIO`); a lazy `error` forced later, such as inside
  `annotateShow`, escapes and crashes the whole suite. Compile-time helpers in `Fixtures` likewise catch
  errors and splice them in as values, so they show up as test failures rather than compile failures.
- When a new TH constructor appears, add it to the generators and the reference functions so the properties
  exercise it, and add a fixture if it affects what `reify` returns.

## Multi-GHC compatibility

Compatibility is the core concern of every change:

- Version differences are handled with CPP, using both `__GLASGOW_HASKELL__` guards and
  `MIN_VERSION_template_haskell(...)`. Follow the surrounding style when adding cases for new TH
  constructors. Two styles are used to avoid silently mishandling new TH constructors: `substVarsWith` and
  `headOfType` enumerate constructors explicitly, while `occursInType` and `namesBoundInPat` handle only the
  binding constructors explicitly and recurse into everything else generically with `syb` (`gmapQ`/`cast`).
  Avoid adding `_ -> default` catch-alls that would quietly return a wrong answer.
- Before GHC 9.0, `TyVarBndr_ flag` is a synonym that ignores `flag`, so a constraint mentioning only `flag`
  (e.g. `Data flag =>`) is ambiguous on older compilers.
- `th-abstraction`'s `Language.Haskell.TH.Datatype.TyVarBndr` is used to paper over the `TyVarBndr` flag
  changes (`TyVarBndr_`, `TyVarBndrSpec`, `TyVarBndrUnit`, `tvName`).
- When adding support for a new GHC: add it to `tested-with` in `th-extras.cabal`, bump the upper bounds on
  `template-haskell` / `th-abstraction` / `containers` as needed, then regenerate CI with `haskell-ci regenerate`
  (the workflow file is generated; don't hand-edit it).

## Releases

Never bump `version:` in `th-extras.cabal` and never modify `ChangeLog.md`; the maintainer handles releases.
