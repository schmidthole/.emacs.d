local client
for _, path in ipairs({ "/opt/homebrew/bin/emacsclient", "/usr/local/bin/emacsclient" }) do
	if hs.fs.attributes(path, "mode") == "file" then
		client = path
		break
	end
end

hs.autoLaunch(true)
hs.urlevent.bind("taymacs-reload", function()
	hs.reload()
end)

local task
hs.hotkey.bind({ "alt" }, "space", function()
	if task and task:isRunning() then
		return
	end
	if not client then
		hs.alert.show("emacsclient is missing; run the repository installer")
		return
	end
	local previous = hs.application.frontmostApplication()
	task = hs.task.new(client, function(code, output, errors)
		task = nil
		if code ~= 0 then
			hs.alert.show("application launcher failed; see the hammerspoon console")
			print((errors or output or "launcher error"):lower())
		end
		if previous and (code ~= 0 or output:match("^nil%s*$")) then
			previous:activate()
		end
	end, {
		"--alternate-editor=",
		"--eval",
		'(progn (require \'mac-launcher (expand-file-name "user-lisp/mac-launcher.el" user-emacs-directory)) (mac-launcher))',
	})
	if not task then
		hs.alert.show("could not create emacsclient task")
		return
	end
	local environment = task:environment()
	environment.PATH = client:match("^(.*)/") .. ":" .. (environment.PATH or "/usr/bin:/bin")
	task:setEnvironment(environment)
	if not task:start() then
		task = nil
		hs.alert.show("could not start emacsclient")
	end
end)
