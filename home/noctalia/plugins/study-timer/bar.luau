--!nonstrict

local GLYPHS = { stopwatch = "stopwatch", timer = "hourglass" }
local COLORS = { paused = "on_surface_variant", finished = "error" }

local function render(view)
  if not view or view.state == "idle" then
    barWidget.render(ui.row({ align = "center" }, {
      ui.glyph({ name = GLYPHS.stopwatch }),
    }))
    return
  end
  local color = COLORS[view.state]
  barWidget.render(ui.row({ gap = 6, align = "center" }, {
    ui.glyph({ name = GLYPHS[view.mode], color = color }),
    ui.label({ text = view.text, color = color }),
  }))
end

function update()
  render(noctalia.state.get("session"))
end

noctalia.state.watch("session", render)

function onClick()
  noctalia.togglePanel("alex/study-timer:panel")
end
