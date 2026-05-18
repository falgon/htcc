---
name: htcc-parser-regression-workflow
description: "Fix htcc parser regressions with focused component tests, malformed-input QA, and nontermination safeguards."
risk: safe
source: project
date_added: "2026-05-18"
---

## Use this skill when

- Editing `src/Htcc/Parser/**` or parser-facing code in `app/Main.hs`.
- Review feedback mentions parse acceptance, parse errors, `ATEmpty`, `manyTill`, EOF, `Megaparsec`, scope restoration, declarations, initializers, or implicit calls.
- A malformed C input can hang, parse successfully by mistake, or produce the wrong diagnostic.

## Do not use this skill when

- The change is pure codegen/output behavior after parsing has already succeeded.

## Investigation steps

1. Read the exact parser function and the nearest combinators it composes with.
2. Search for zero-width success in recursive or repeated contexts: `M.many`, `M.manyTill`, `M.option`, `ATEmpty`, `M.eof`, `M.try`.
3. Identify whether the parser must fail, consume input, or produce `ATEmpty` intentionally.
4. Check existing component tests in `test/Tests/ComponentsTests/Parser/Combinators.hs` before adding new helpers.

## Regression test placement

- Put parser unit regressions in `test/Tests/ComponentsTests/Parser/Combinators.hs` when direct `parseProgram`, `parseAssignExpr`, or helper assertions can reproduce the bug.
- Put CLI parser regressions in subprocess tests only when stderr/exit-code behavior is the relevant contract.
- Prefer adding cases under the existing `Parser.Program.*` group matching the construct, such as `function-call`, `scalar-initializer`, or `function-pointer-arithmetic`.
- For call argument lists, trailing commas such as `f(1,)` are malformed. For initializer lists, check existing accepted C-like trailing-comma behavior before turning a trailing comma into a rejection.
- For initializer EOF regressions, consider both file-scope and local-scope shapes such as `int g =`, `int g = {1`, and `int main(){ int x =`.

## Nontermination safeguards

- Any bug involving EOF, repeated combinators, or `manyTill` must have a timeout-guarded manual QA command, for example:

```bash
printf 'int main(){ return f(' | timeout 5s stack exec htcc -- /dev/stdin
```

- The expected outcome for malformed input is non-zero exit before the timeout, not success.
- Use `timeout` or `gtimeout`, whichever exists on the host, and report the command used.
- Do not fix nontermination by accepting malformed input as `ATEmpty`.

## Verification

- `stylish-haskell -i <all changed .hs files, including app/Main.hs when touched> test/Tests/ComponentsTests/Parser/Combinators.hs`
- `lsp_diagnostics` on edited Haskell files when the diagnostics tool is available. If unavailable, state that and substitute `stack build` plus the relevant test command.
- `stack test --test-arguments components`.
- Full `stack test` for parser-wide behavior or when the change affects common expression/statement parsing.
- `stack build`.
- Manual CLI QA for malformed inputs when user-visible parser behavior or hangs are involved.

## Review focus

- Does the fix remove the bad acceptance path without breaking intended empty statements or empty `for` sections?
- Do the tests cover both minimal malformed shape and comma/trailing-argument shape where applicable?
- Does the parser fail with a parse error rather than looping or producing a misleading successful AST?
