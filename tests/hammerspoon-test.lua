local function setup(options)
	options = options or {}
	local state = { activated = 0, tasks = 0 }
	hs = {
		fs = {
			attributes = function()
				return not options.missing and "file" or nil
			end,
		},
		autoLaunch = function(enabled)
			assert(enabled)
		end,
		urlevent = {
			bind = function(name, callback)
				assert(name == "taymacs-reload")
				state.reload = callback
			end,
		},
		reload = function()
			state.reloaded = true
		end,
		alert = {
			show = function(message)
				state.alert = message
			end,
		},
		application = {
			frontmostApplication = function()
				return {
					activate = function()
						state.activated = state.activated + 1
					end,
				}
			end,
		},
		hotkey = {
			bind = function(modifiers, key, callback)
				assert(#modifiers == 1 and modifiers[1] == "alt" and key == "space")
				state.trigger = callback
			end,
		},
		task = {
			new = function(path, callback, arguments)
				state.tasks = state.tasks + 1
				state.path, state.callback, state.arguments = path, callback, arguments
				if options.creation_failure then
					return nil
				end
				return {
					environment = function()
						return { PATH = "/usr/bin:/bin" }
					end,
					setEnvironment = function(_, environment)
						state.environment = environment
					end,
					isRunning = function()
						return state.running
					end,
					start = function()
						state.running = not options.start_failure
						return state.running
					end,
				}
			end,
		},
	}
	dofile("config/hammerspoon/taymacs.lua")
	return state
end

local state = setup()
state.reload()
assert(state.reloaded)
state.trigger()
assert(state.path == "/opt/homebrew/bin/emacsclient")
assert(state.arguments[1] == "--alternate-editor=" and state.arguments[2] == "--eval")
assert(state.arguments[3]:match("mac%-launcher"))
assert(state.environment.PATH == "/opt/homebrew/bin:/usr/bin:/bin")
state.trigger()
assert(state.tasks == 1)
state.running = false
state.callback(0, "nil\n", "")
assert(state.activated == 1)
state.trigger()
assert(state.tasks == 2)
state.running = false
state.callback(0, "t\n", "")
assert(state.activated == 1)

for _, options in ipairs({ { missing = true }, { creation_failure = true }, { start_failure = true } }) do
	state = setup(options)
	state.trigger()
	assert(state.alert)
end

state = setup()
state.trigger()
state.running = false
state.callback(1, "", "test failure")
assert(state.alert and state.activated == 1)
print("hammerspoon launcher tests passed")
