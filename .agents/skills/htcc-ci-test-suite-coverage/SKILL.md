---
name: htcc-ci-test-suite-coverage
description: "Keep htcc CI workflows aligned with the test runner command matrix, especially explicit self/components/subp/docker lists."
risk: safe
source: project
date_added: "2026-05-18"
---

## Use this skill when

- Editing `.github/workflows/main.yml`, `.travis.yml`, Docker CI scripts, or the htcc test command selector.
- Adding or renaming test commands such as `self`, `components`, `subp`, or `docker`.
- Review feedback says a suite runs by default locally but is skipped in CI because CI enumerates commands explicitly.

## Do not use this skill when

- The task is unrelated to CI/test command coverage.

## Required checks

1. Search all CI command lists for explicit `stack test --test-arguments ...` invocations.
2. Compare those lists to `commandsToRun` / command-selection behavior in the test runner.
3. If a workflow lists `self` and `subp` explicitly, ensure `components` is also listed when component tests are required.
4. Do not assume default command selection applies to CI steps that enumerate commands.

## Known CI locations

- `.github/workflows/main.yml`: GitHub Actions compiler tests.
- `.travis.yml`: Travis compiler tests.
- Docker-specific commands may remain separate: `stack test --test-arguments docker` and clean variants.
- The Docker clean test command is `stack test --test-arguments docker --test-arguments --clean`.
- README examples are documentation, not CI enforcement, unless the task explicitly asks to update docs.

## Verification

- Run the newly added command locally, for example:

```bash
stack test --test-arguments components
```

- Run `git diff --check`.
- Parse changed YAML files. If `yaml-ls` is unavailable, use:

```bash
ruby -e 'require "yaml"; YAML.load_file(".github/workflows/main.yml"); YAML.load_file(".travis.yml"); puts "yaml ok"'
```

- Full `stack test` is optional for CI-only command-list changes unless the test runner itself changed.
- Run Docker commands only when Docker is available locally. If unavailable, report them as unavailable and rely on CI definition inspection/YAML parsing rather than claiming local Docker success.

## Review focus

- Every explicit CI compiler-test list includes the required suite.
- Added commands are in the same stage as related compiler tests.
- No secrets, permissions, or checkout credentials are broadened.
- Docker commands are not mixed into non-Docker stages.
