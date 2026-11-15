--!nonstrict

local PRESET_MINUTES = { 10, 20, 45 }
local CAPTIONS = {
  stopwatch = { running = "Stopwatch", paused = "Stopwatch paused" },
  timer = { running = "Timer", paused = "Timer paused", finished = "Time is up" },
}
local TIME_COLORS = { running = "primary", paused = "on_surface_variant", finished = "error" }

local customMinutes = nil
-- A new key gives the input a fresh, focused native control on each open.
local inputKey = 0

local function send(name, minutes)
  noctalia.state.set("cmd", { name = name, minutes = minutes })
end

local function startAndClose(name, minutes)
  send(name, minutes)
  panel.close()
end

local function startCustomTimer()
  if customMinutes then
    startAndClose("timer", customMinutes)
  end
end

local function setupView()
  local presets = {}
  for _, minutes in PRESET_MINUTES do
    table.insert(presets, ui.button({
      key = `preset-{minutes}`,
      text = `{minutes} min`,
      variant = "secondary",
      flexGrow = 1,
      onClick = function()
        startAndClose("timer", minutes)
      end,
    }))
  end

  return ui.column({ gap = 12, padding = 16 }, {
    ui.button({
      text = "Start stopwatch",
      glyph = "stopwatch",
      variant = "primary",
      onClick = function()
        startAndClose("stopwatch")
      end,
    }),
    ui.separator({}),
    ui.label({ text = "Timer", fontWeight = "bold" }),
    ui.row({ gap = 8 }, presets),
    ui.row({ gap = 8, align = "center" }, {
      ui.input({
        key = `minutes-{inputKey}`,
        placeholder = "Minutes",
        focus = true,
        flexGrow = 1,
        onChange = function(text)
          local minutes = tonumber(text)
          customMinutes = minutes and minutes > 0 and minutes < math.huge and minutes or nil
        end,
        onSubmit = startCustomTimer,
      }),
      ui.button({ text = "Start", variant = "secondary", onClick = startCustomTimer }),
    }),
  })
end

local function sessionView(view)
  local actions
  if view.state == "finished" then
    actions = {
      ui.button({
        text = "Dismiss",
        variant = "primary",
        flexGrow = 1,
        onClick = function()
          send("reset")
        end,
      }),
    }
  else
    actions = {
      ui.button({
        text = "Reset",
        variant = "destructive",
        flexGrow = 1,
        onClick = function()
          send("reset")
        end,
      }),
      ui.button({
        text = view.state == "running" and "Pause" or "Resume",
        variant = "primary",
        flexGrow = 1,
        onClick = function()
          send("toggle")
        end,
      }),
    }
  end

  return ui.column({ gap = 12, padding = 16 }, {
    ui.row({ justify = "center" }, {
      ui.label({ text = CAPTIONS[view.mode][view.state], color = "on_surface_variant" }),
    }),
    ui.row({ justify = "center" }, {
      ui.label({ text = view.text, fontSize = 36, fontWeight = "bold", color = TIME_COLORS[view.state] }),
    }),
    ui.progress({ progress = view.progress or 0, height = 6, radius = 3, visible = view.mode == "timer" }),
    ui.row({ gap = 8 }, actions),
  })
end

local function render(view)
  if not view or view.state == "idle" then
    panel.render(setupView())
  else
    panel.render(sessionView(view))
  end
end

function onOpen()
  customMinutes = nil
  inputKey += 1
  render(noctalia.state.get("session"))
end

noctalia.state.watch("session", render)
