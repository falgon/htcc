---
name: htcc-verification-matrix
description: "Choose and run the correct htcc verification commands for parser, CLI, CI, output permission, Docker, and documentation changes."
risk: safe
source: project
date_added: "2026-05-18"
---

## Use this skill when

- You edited htcc code, tests, CI, scripts, examples, or documentation and need to know what to run.
- You are preparing a final report after implementation.

## Do not use this skill when

- No files changed and the user only asked for discussion.

## Universal checks

- Haskell edits: `stylish-haskell -i <changed .hs files>`.
- Haskell edits: `lsp_diagnostics` on each changed `.hs` file when the diagnostics tool is available. If unavailable, state that and substitute `stack build` plus the relevant tests.
- Any edit: `git diff --check`.
- YAML edits: parse `.yml` / `.yaml` files. If `yaml-ls` is unavailable, use `ruby -e 'require "yaml"; YAML.load_file("path")'` or equivalent.
- Before final answer: `git status --short` and report uncommitted files.

## Change-type matrix

| Change type | Required tests | Manual QA |
|---|---|---|
| Parser combinators / AST typing | `stack test --test-arguments components`; full `stack test` for core parser behavior | malformed input through `stack exec htcc -- /dev/stdin`, often with `timeout` |
| CLI diagnostics / app/Main.hs | relevant subprocess tests or full `stack test` | run `stack exec htcc -- ...`, capture exit code, stdout, stderr |
| Output replacement / permissions | `stack test --test-arguments components`; full `stack test` if behavior is shared | create temp files/dirs with modes, run `htcc -o`, verify output and modes |
| CI workflow or Travis | `stack test --test-arguments <added command>`; YAML parse | inspect `git diff` and ensure every explicit list includes intended command |
| Docker command behavior | `stack test --test-arguments docker` and `stack test --test-arguments docker --test-arguments --clean` when Docker is available | do not fake Docker success; report unavailable Docker explicitly |
| Examples | `cd example && make && cd dist && ./knapsack && ./merge_sorting_linked_list && ./shuffle_and_sort`; `cd example && make docker && make clean_docker` only when Docker is available | execute generated examples when requested or affected |
| Documentation only | no Haskell formatter unless code changed; run referenced commands when they are the contract and feasible | check links/commands touched by the edit when practical |

## Evidence standard

- A passing unit test is not enough when user-visible CLI behavior changed; run the CLI.
- A parser test is not enough for hang bugs; include timeout-guarded manual QA.
- Use `timeout` or `gtimeout`, whichever exists on the host, and report the command used.
- A successful build is not enough for CI edits; parse YAML and run the newly added command locally when possible.
- When Docker is unavailable, substitute Docker checks with non-Docker example/build tests where relevant plus command/diff inspection, then report Docker behavior as residual risk.
- If a tool is missing, state the exact missing tool and the substitute check.

## Reporting template

```text
Changed: <paths>
Verification: <commands and pass/fail>
Manual QA: <input, exit code, key stdout/stderr or output-file observation>
Review: <reviewer outcomes>
Remaining: <none or explicit follow-up>
```
