--!nonstrict
-- Owns the session. The bar widget and panel only render what it publishes.

local ALARM_SECONDS = 10
local ALARM_SOUND = "alarm"
local SESSION_PATH = assert(noctalia.pluginDataDir()) .. "/session.json"

-- state: idle | running | paused | finished
-- Times are wall-clock so a session survives shell restarts and suspend.
local session = { state = "idle" }
local alarmUntilMs = 0
local alarmReady = false
local publishedText = nil

local function elapsedMs()
  local currentRun = session.state == "running" and noctalia.nowMs() - session.startedAtMs or 0
  return session.accumulatedMs + currentRun
end

local function formatDuration(totalSeconds)
  local hours = totalSeconds // 3600
  local minutes = totalSeconds // 60 % 60
  local seconds = totalSeconds % 60
  if hours > 0 then
    return string.format("%d:%02d:%02d", hours, minutes, seconds)
  end
  return string.format("%d:%02d", minutes, seconds)
end

local function buildView()
  if session.state == "idle" then
    return { state = "idle", text = "" }
  end
  local view = { state = session.state, mode = session.mode }
  if session.mode == "timer" then
    local remainingMs = math.max(0, session.durationMs - elapsedMs())
    view.text = formatDuration(math.ceil(remainingMs / 1000))
    view.progress = remainingMs / session.durationMs
  else
    view.text = formatDuration(math.floor(elapsedMs() / 1000))
  end
  return view
end

local function publish()
  local view = buildView()
  publishedText = view.text
  noctalia.state.set("session", view)
end

local function commit()
  local ok, err = noctalia.writeFile(SESSION_PATH, noctalia.json.encode(session))
  if not ok then
    noctalia.log(`Could not save session: {err}`)
  end
  -- Sub-second ticks keep the display from skipping when they drift off the second boundary.
  noctalia.setUpdateInterval(session.state == "running" and 250 or 1000)
  publish()
end

local function playAlarm()
  if alarmReady and noctalia.nowMs() < alarmUntilMs then
    noctalia.sound.play(ALARM_SOUND)
  end
end

local function finish()
  session.state = "finished"
  session.accumulatedMs = session.durationMs
  alarmUntilMs = noctalia.nowMs() + ALARM_SECONDS * 1000
  noctalia.notify("Study timer", "Time is up!")
  playAlarm()
  commit()
end

local function start(mode, durationMs)
  if session.state ~= "idle" and session.state ~= "finished" then
    return
  end
  alarmUntilMs = 0
  session = {
    state = "running",
    mode = mode,
    durationMs = durationMs,
    accumulatedMs = 0,
    startedAtMs = noctalia.nowMs(),
  }
  commit()
end

local function reset()
  alarmUntilMs = 0
  session = { state = "idle" }
  commit()
end

local function toggle()
  if session.state == "idle" then
    start("stopwatch")
  elseif session.state == "running" then
    session.accumulatedMs = elapsedMs()
    session.state = "paused"
    commit()
  elseif session.state == "paused" then
    session.startedAtMs = noctalia.nowMs()
    session.state = "running"
    commit()
  else
    reset()
  end
end

local function run(command, minutes)
  if command == "stopwatch" then
    start("stopwatch")
  elseif command == "timer" then
    if minutes and minutes > 0 and minutes < math.huge then
      start("timer", math.floor(minutes * 60000))
    end
  elseif command == "toggle" then
    toggle()
  elseif command == "reset" then
    reset()
  end
end

local function restore()
  local raw = noctalia.readFile(SESSION_PATH)
  if not raw then
    return
  end
  local ok, saved = pcall(noctalia.json.decode, raw)
  if ok and type(saved) == "table" and type(saved.state) == "string" then
    session = saved
  else
    noctalia.log("Ignoring unreadable saved session")
  end
end

function update()
  if session.state == "running" then
    if session.mode == "timer" and elapsedMs() >= session.durationMs then
      finish()
    elseif buildView().text ~= publishedText then
      publish()
    end
  elseif session.state == "finished" then
    playAlarm()
  end
end

-- noctalia msg plugin alex/study-timer:service all <stopwatch|timer <minutes>|toggle|reset>
function onIpc(event, payload)
  run(event, tonumber(payload))
end

noctalia.state.watch("cmd", function(cmd)
  run(cmd.name, cmd.minutes)
end)

noctalia.sound.load(ALARM_SOUND, "sounds/alarm.wav", function(ok, err)
  if ok then
    alarmReady = true
  else
    noctalia.log(`Could not load alarm sound: {err}`)
  end
end)

restore()
noctalia.setUpdateInterval(session.state == "running" and 250 or 1000)
publish()
