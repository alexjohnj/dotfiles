---
name: herdr-app
description: "Use herdr-app whenever you want to change what the user sees in the Herdr window: spaces, tabs, panes, layouts, focus, zoom, what's shown, or building Herdr mods. For running or coordinating agents, background tasks or terminals, use herdr-core (hc) instead."
---

This skill is for `herdr-app`, the command that drives the Herdr window. It is
not `herdr-core` (`hc`), which runs terminals and agents: use the `herdr-core`
skill for that.

If `$HC_TERMINAL_ID` is set, you're running inside herdr-core, so herdr-app
applies. If not, ask the user once this session whether to use it.

Before using herdr-app, run `herdr-app --skill` and follow it.
