# code-style-guide

A babashka CLI tool that enforces coding style rules across polyglot codebases.

## Quick Start

```bash
bb check -- --dir ./my-project
bb check -- --dir ./my-project --format json --severity medium
```

## Requirements

- [Babashka](https://github.com/babashka/babashka) v1.12+

## Rules

| Rule | Severity | Fixable | Description |
|------|----------|---------|-------------|
| line-length | low | no | Lines over 120 chars |
| trailing-whitespace | low | yes | Trailing spaces/tabs |
| tabs-not-spaces | medium | yes | Tab indentation |
| consecutive-blank-lines | low | no | Multiple blank lines |
| todo-without-ticket | medium | no | TODOs lacking ticket refs |
| fixme-present | high | no | FIXME comments |
| debug-print | high | no | print/console.log/fmt.Print |
| magic-number | medium | no | Unnamed numeric constants |

## Supported Languages

Python, JavaScript/TypeScript, Go, Java, Ruby, Clojure, Rust, Shell

## Exit Codes

- `0` — clean
- `1` — violations found
- `2` — error
