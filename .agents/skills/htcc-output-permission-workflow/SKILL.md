---
name: htcc-output-permission-workflow
description: "Work safely on htcc output replacement, fallback copy, file mode preservation, rollback, symlink, and hard-link behavior."
risk: safe
source: project
date_added: "2026-05-18"
---

## Use this skill when

- Editing `src/Htcc/Output.hs` or code that writes `-o` outputs, visualization outputs, staging files, backups, or direct replacement paths.
- Review feedback mentions `setFileMode`, owner/group/other permissions, read-only directories, fallback copies, stale output, symlinks, hard links, or rollback.

## Do not use this skill when

- The output path is not touched and the task is only parser/codegen logic.

## Key invariants

- Existing output content must not be lost on failed replacement.
- Existing output mode should be preserved unless the mode strategy intentionally changes executable bits.
- Direct fallback must not preemptively `chmod` when a write/copy could already succeed through group or other permissions.
- Hard-linked outputs must remain protected by `ensureInPlaceReplacementSafe`.
- Symlink behavior must match existing resolution rules; do not add broad symlink rewrites while fixing permission bugs.

## Test patterns

- Component tests belong in `test/Tests/ComponentsTests/AsmOutput.hs` near existing fallback/permission tests.
- Use custom `copyReplacementOutput` functions to assert ordering, mode at copy time, partial-write rollback, or failure surfacing.
- Use temp files from `openTempFile`, close handles before mode changes, and cleanup with `catchIOError`-guarded removal.
- Test file modes with `fileMode <$> getFileStatus` and `intersectFileModes replacedMode 0o777`.

## Manual QA patterns

- For fallback replacement, create a read-only directory so staging in the output directory fails and direct replacement is exercised.
- Verify:
  - command exit status,
  - stdout/stderr,
  - output file contains expected assembly such as `.global main`,
  - stale output is preserved when failure is expected,
  - file mode is restored when mode behavior is the contract.

Example shape:

```bash
tmpdir=$(mktemp -d /tmp/htcc-output-fallback.XXXXXX)
target="$tmpdir/out.s"
cleanup() {
  chmod u+rw "$target" 2>/dev/null || true
  chmod u+rwx "$tmpdir" 2>/dev/null || true
  rm -rf -- "$tmpdir"
}
trap cleanup EXIT
printf 'stale output\n' > "$target"
chmod 444 "$target"
chmod 555 "$tmpdir"
printf 'int main(void) { return 0; }\n' | stack exec htcc -- -o "$target" /dev/stdin
```

Always restore permissions before cleanup; use a `trap` so interrupted QA does not leave read-only temp paths behind.

## Verification

- `stylish-haskell -i src/Htcc/Output.hs test/Tests/ComponentsTests/AsmOutput.hs`
- `lsp_diagnostics` on both files when the diagnostics tool is available. If unavailable, state that and substitute `stack build` plus the relevant test command.
- `stack test --test-arguments components`.
- Full `stack test` when output replacement affects subprocess behavior.
- `stack build`.
- Manual permission/fallback QA.

## Review focus

- Ordering of `copy`, `chmod`, `restore`, and `rollback`.
- PermissionError-only fallback; non-permission errors should not be silently retried as permission problems.
- Data loss during partial replacement and restoration failure reporting.
