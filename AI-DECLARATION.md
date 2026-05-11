---
version: "0.1.2"
level: pair
processes:
  design: hint
  implementation: assist
  testing: pair
  documentation: pair
---

This format is based on [AI-DECLARATION.md](https://ai-declaration.md/en/0.1.2).

## Notes

- AI tooling: [Claude Code](https://claude.com/claude-code) with Claude Opus 4.7 (1M context).
- Design and architectural decisions are human-led; AI surfaces suggestions passively during exploration.
- Implementation is human-driven with AI assisting on focused sub-tasks under prompt.
- Tests and documentation are written collaboratively: human and AI both act on the task, with the human shaping and understanding the result throughout.
- AI usage concentrates on chores (e.g. version bumps, changelog entries), systematic refactors that are hard to script, and filling in tests to reach 100% coverage.
- AI is not used to invent new idioms or concepts in the code; all AI-produced code is human-reviewed before landing.
- Processes not listed (e.g. review, deployment) default to `none`.

## Cooperation style

> Iterative drafting with substantive critique on each round, where the human directs framing and verifies against primary sources, and the AI handles wording and mechanical edits.
