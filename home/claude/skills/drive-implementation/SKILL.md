---
name: drive-implementation
description: "Drive the implementation of a request."
disable-model-invocation: true
---

You have been provided the description of something to build.

The goal is to drive the entire development of the feature.

Every **child agent** below is an interactive `claude` you orchestrate with
Rex. Call the Skill tool with `rex` before your first Rex command.

Communication to and from child agents should be sparse. Communicate primarily
through **context pointers**: to the spec, tickets, research notes, and previous
commits. Don't duplicate information already available via pointers.

### Workspace

- Each invocation of this skill uses your own Rex session (`$REX_SESSION`),
  renamed after the feature. Rex can't move panes between sessions, so don't
  create a new one. Do this before launching the first child; this skill is
  the user's request to rename the session and add tabs.
- Your own pane stays in its current tab. Each repository the feature touches
  gets its own tab in the session, named after the repository. Children
  working in a repository appear as panes in that repository's tab.
- Always start children inside this session, never in a new one.

### Child agents

- Start each child as a pane in its repository's tab without taking focus,
  labelled after its role (e.g. `explore`, `impl-<slug>`, `verify`), running
  `claude` with its first task as the prompt.
- Launch each child in the repository it works in, so it picks up that
  repository's `AGENTS.md`, skills, hooks and settings.
- Each task names its scope, the skill to call, and where to write long
  output. It permits the child to start subagents and asks it to report back
  when done or stuck by sending a one-line message to your pane
  (`$REX_SESSION` and `$REX_BLOCK`) with the `rex` skill.
- After launching children, end your turn and wait for their reports. Then
  read the actual result (notes file, commits) before acting on a report.
  Capture a child's pane if it has gone quiet.

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
     used as their working directory. Once all are finished, integrate their
     commits onto the work branch and remove the worktrees.
   - Each implementer calls the Skill tool with `tdd` to build the ticket, and
     commits its work before reporting done.
5. Once all implementers are finished, call the Skill tool with `code-review` on
   the work branch. Fix all issues raised by the code review in a single
   `impl-fixes` child agent.
6. Once code review is complete, launch a `verify` child agent. It validates the
   work using the pre-agreed verification steps, and saves its screenshots and
   a `verification.md` to the notes directory.
7. If the `verify` child agent flags any issues, go back to step 4. and address
   them with an implementer child agent.
8. When verification has finished, the working tree should be clean with all
   work committed. Close every child pane and repository tab you started. Write
   `report.html` to the notes directory and `open` it. It covers:
   1. Relevant implementation notes.
   2. Anything that has diverged from what was originally scoped.
   3. Anything for the user to action or decide.
   4. Verification evidence, with screenshots referenced by relative path.
   5. The branch, commits, and pointers to the spec and tickets.

   Call the Skill tool with `artifact-design` first and follow its page
   contract, so the page can be published unchanged. Don't publish it unless
   the user asks. Notify the user with a short summary and the file path.

### Decisions

When a decision for the user turns on something visual (layouts, screenshots,
perf numbers), write `decision-<slug>.html` to the notes directory the same way
and `open` it, then ask in the terminal.

### Style

- Squash code review and verification fix commits into the main commits for the feature.
