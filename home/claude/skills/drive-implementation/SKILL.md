---
name: drive-implementation
description: "Drive the implementation of a request."
disable-model-invocation: true
---

You have been provided the description of something to build.

The goal is to drive the entire development of the feature.

Every **child agent** below is an interactive `claude` you orchestrate with
Herdr: herdr-core (`hc`) starts, messages, waits on and removes agents;
herdr-app arranges what the user sees. Before launching the first child, run
`herdr-app --skill` and `herdr-core --skill` and follow their current usage.

Communication to and from child agents should be sparse. Communicate primarily
through **context pointers**: to the spec, tickets, research notes, and previous
commits. Don't duplicate information already available via pointers.

### Workspace

- Each invocation of this skill gets its own Herdr space, named after the
  feature. You (the orchestrator) are already running in a pane somewhere:
  move yourself into the new space rather than leaving yourself behind in the
  old one, so the space holds both you and your children. Rename an existing
  space instead of creating a second one if you are already alone in one.
  Do this before launching the first child; this skill is the user's request
  to rearrange the window.
- Your own pane stays in the space's first tab. Each repository the feature
  touches gets its own tab in that space. Children working in a repository
  appear in that repository's tab.
- Always start children inside this space, never in a new one.

### Child agents

- Start each child with herdr-core in the background, named after its role
  (e.g. `explore`, `impl-<slug>`, `verify`), with its first task in the launch.
- Launch each child in the repository it works in, so it picks up that
  repository's `AGENTS.md`, skills, hooks and settings.
- Each task names its scope, the skill to call, and where to write long
  output. It permits the child to start subagents and asks it to report back
  to your terminal address when done or stuck.
- Wait on your children with herdr-core, then read the actual result (notes
  file, commits) before acting on a report.

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
   work using the pre-agreed verification steps.
7. If the `verify` child agent flags any issues, go back to step 4. and address
   them with an implementer child agent.
8. When verification has finished, the working tree should be clean with all
   work committed. Stop and remove every child agent you started. Notify the
   user, reporting:
   1. Relevant implementation notes.
   2. Anything that has diverged from what was originally scoped.
   3. Anything for the user to action or decide.

### Style

- Squash code review and verification fix commits into the main commits for the feature.
