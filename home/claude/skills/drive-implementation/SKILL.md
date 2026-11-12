---
name: drive-implementation
description: "Drive the implementation of a request."
disable-model-invocation: true
---

You have been provided the description of something to build.

The goal is to drive the entire development of the feature.

Every **child agent** below is an interactive `claude` started with herdr-core
(`hc`). Call the Skill tool with `herdr-core` before launching the first one.

Communication to and from child agents should be sparse. Communicate primarily
through **context pointers**: to the spec, tickets, research notes, and previous
commits. Don't duplicate information already available via pointers.

### Launching a child agent

```sh
hc run --bg --name <name> --cwd <dir> claude "<task> You may start subagents. Report with hc send $HC_TERMINAL_ID when done or stuck."
```

- Your shell expands `$HC_TERMINAL_ID`, so the child receives your literal ID.
- Each task names its scope, the skill to call, and where to write long output.
- Collect results with `hc wait --children all`, then read the actual result
  (notes file, commits) before acting on a report.
- Launch child agents in the repository they will work in so they use that
  repository's `AGENTS.md`, skills hooks and settings.

### Steps

1. Review the description you've been given. The description should include:
   1. A clear idea of what the end goal is.
   2. All resources needed (e.g., designs, specs etc.) to achieve the goal.
   3. Clear instructions on how to validate the change.
2. (optional) Launch an `explore` child agent to conduct any exploration
   required by the ticket - relevant codebase files or external
   documentation. It saves markdown notes in a directory outside the repo,
   accessible by all future child agents. This lets implementer child agents
   focus on implementation rather than exploration.
3. Create a work branch (`alexj/<slug>`) if not already on a suitable branch.
4. Launch `impl-<slug>` child agents to implement the work:
   - A lone implementer runs in the work branch's checkout.
   - Parallel implementers each get their own worktree off the work branch:
     `git worktree add ~/src/plex/worktrees/<repo>/<slug> -b alexj/<slug> <work-branch>`,
     passed as `--cwd`. Once all are finished, integrate their commits onto the
     work branch and remove the worktrees.
   - Each implementer calls the Skill tool with `tdd` to build the ticket, and
     commits its work before reporting done.
5. Once all implementers are finished, call the Skill tool with `code-review` on
   the work branch. Fix all issues raised by the code review in a single
   `impl-fixes` child agent.
6. Once code review is complete, launch a `verify` child agent. It validates the
   work using the pre-agreed verification steps.
7. If the `verify` child agent flags any issues, go back to step 4. and address
   them with an implementer child agent.
8. When verification has finished, the working tree should be clean with all
   work committed. `hc kill` then `hc rm` every child agent you started. Notify
   the user, reporting:
   1. Relevant implementation notes.
   2. Anything that has diverged from what was originally scoped.
   3. Anything for the user to action or decide.

### Style

- Squash code review and verification fix commits into the main commits for the feature.
