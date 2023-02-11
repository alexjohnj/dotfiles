local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local theme = {}

local colors = {
   primary_background = "#222222",
   primary_foreground = "#dbd0f3",
   text_on_foreground = "#111",
   text_on_background = "#ddd",
   accent_color = "#a669db"
}


theme.font = "Overpass Mono 10"

theme.bg_normal = colors.primary_background
theme.bg_focus = colors.accent_color
theme.bg_urgent = "#ff0000"
theme.bg_minimize = "#444444"

theme.fg_normal = colors.text_on_background
theme.fg_focus = colors.text_on_foreground
theme.fg_urgent = "#ffffff"
theme.fg_minimize = "#ffffff"

theme.useless_gap = 8
theme.border_width = 0.5
theme.border_normal = theme.bg_normal
theme.border_focus = colors.accent_color
theme.border_marked = "#91231c"

theme.hotkeys_bg = colors.primary_background
theme.hotkeys_fg = colors.text_on_background
theme.hotkeys_description_font = "Overpass Mono"
theme.hotkeys_font = "Overpass Mono Bold"
theme.hotkeys_opacity = 0.92
theme.hotkeys_modifiers_fg = "#d5c4a1" -- white
theme.hotkeys_label_bg = colors.accent_color
theme.hotkeys_label_fg = "black"
theme.hotkeys_group_margin = 30

theme.taglist_fg_empty = "#87af87"
theme.taglist_fg_occupied = "#d3859a"
theme.taglist_fg_focus = "#d79920"
theme.taglist_bg_focus = ""

theme.notification_width = 400
theme.notification_bg = colors.primary_background
theme.notification_fg = colors.text_on_background
theme.notification_border_color = colors.accent_color
theme.menu_submenu = ""
theme.menu_height = dpi(20)
theme.menu_width = dpi(105)

return theme
