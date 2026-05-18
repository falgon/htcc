---
name: htcc-review-comment-response
description: "Handle htcc review comments end-to-end: reproduce, judge validity, apply minimal fixes, verify, review, and prepare commit/push."
risk: safe
source: project
date_added: "2026-05-18"
---

## Use this skill when

- Addressing GitHub, Codex, or reviewer comments against this htcc repository.
- The comment points at parser, CLI, CI, codegen, output replacement, Docker, or test behavior.
- You need to decide whether a review finding is valid before changing code.

## Do not use this skill when

- The task is only to explain a comment without changing or verifying anything.
- The task is a generic Haskell question unrelated to htcc review feedback.

## Core workflow

1. Read the exact reviewed file and nearby code before judging the comment.
2. Reproduce or reason from the current diff; do not assume the reviewer is right.
3. If valid, make the smallest behavior-preserving fix that addresses exactly the comment.
4. Add or extend a regression test that fails on the reviewed bug shape.
5. Run the htcc verification matrix for the touched area.
6. Launch independent review agents after implementation:
   - at least two general/code reviewers for behavior and tests,
   - one security/operational reviewer.
7. Wait 180-300 seconds when reviewer results are a prerequisite. Treat `No agents completed yet` as pending, not failure, and do not mark review complete while required reviewers are still unfinished.
8. If a reviewer returns empty output, retry or report that reviewer as unavailable; do not silently replace a required review with an unrelated fallback.
9. If all review findings are resolved and the user explicitly asks for a commit, commit with `--no-gpg-sign` and follow the current repo/user rules for trailers. Do not add co-author or other trailers unless the current repo/user rules or the user explicitly require them. Push only after explicit request.

## GitHub review threads

- When a GitHub PR URL or unresolved review threads are part of the task, use `gh` or an available GitHub connector to fetch PR metadata, inline review context, and current diff before editing.
- If thread resolution state matters, prefer a GitHub connector or `gh` GraphQL query that exposes unresolved/resolved thread state; flat review comments alone are not enough.
- Stop before commit or push when the user asks to fix comments without publishing local changes.

## Validity checklist

- Does the comment cite reachable code on this branch?
- Does the described bad behavior still exist after earlier commits?
- Can a focused test or timeout-guarded command distinguish old and fixed behavior?
- Is the requested behavior consistent with existing htcc patterns?
- Would the fix broaden scope beyond the review comment? If yes, reduce it.

## Verification requirements

- Always run `stylish-haskell -i` for edited Haskell files.
- Always run `lsp_diagnostics` on edited Haskell files when the diagnostics tool is available. If unavailable, state that and substitute `stack build` plus the relevant test command.
- Always run the smallest relevant `stack test --test-arguments ...` command, then full `stack test` when parser/core behavior changed.
- Always run a real CLI/manual QA command when the change affects the executable, output files, CI commands, or malformed-input behavior.
- Report any unavailable verifier explicitly, with the substitute used.

## Common htcc review patterns

- Parser nontermination or malformed input: use `timeout` or `gtimeout` around `stack exec htcc -- /dev/stdin` and add component parser tests.
- CLI diagnostics: assert stderr text, exit code, stdout absence, and absence of Haskell exception internals such as `HasCallStack`.
- Output replacement and permission handling: test file modes, stale-output preservation, rollback, symlink/hard-link safety, and manual read-only directory scenarios.
- CI test coverage: check every explicit CI command list; defaults in `commandsToRun` do not affect workflows that enumerate commands.

## Output format

- State whether each review comment was valid.
- Summarize the minimal fix and regression test.
- List verification commands and outcomes.
- List reviewer outcomes and any declined findings with reasons.
