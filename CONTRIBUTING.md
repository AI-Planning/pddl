# Contributing

Contributions are welcome, and greatly appreciated! Every little bit helps, and credit will always be given.

If you need support, want to report/fix a bug, ask for/implement features, you can check the
[Issues page](https://github.com/AI-Planning/pddl/issues)
or [submit a Pull request](https://github.com/AI-Planning/pddl/pulls).

For other kinds of feedback, you can contact one of the [authors](./authors.md) by email.

## Development setup

Install [uv](https://docs.astral.sh/uv/), then:
- `uv sync --dev` - install dependencies;
- `uv run pre-commit install` - enable the git hooks (ruff + file hygiene);
- `uv run pre-commit run --all-files` - run all hooks once.
