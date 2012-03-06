
--x = 0

g = love.graphics
k = love.keyboard

canvas = {}

player = {}
player.loc = {}
player.loc.x = 0 -- X coords not used yet TODO
player.loc.y = 0
player.height = 20
player.dir = 1
player.rot = 0
player.rotSpeed = 1/2
player.maxSpeed = 12
player.color = {0, 0, 255}
player.ring = 0

enemies = nil
Enemy = {}

spawnWait = 2 -- seconds
spawnLast = 99999 -- some arbitrary max value - no last spawn

help = 10
debug = 10
lastDt = 0

colors = {}
colors[ 1] = {0xFF, 0x00, 0xFF}
colors[ 2] = {0xFF, 0x00, 0xCC}
colors[ 3] = {0xFF, 0x00, 0x99}
colors[ 4] = {0xFF, 0x00, 0x66}
colors[ 5] = {0xFF, 0x00, 0x33}
colors[ 6] = {0xFF, 0x00, 0x00}
colors[ 7] = {0xFF, 0x33, 0x00}
colors[ 8] = {0xFF, 0x66, 0x00}
colors[ 9] = {0xFF, 0x99, 0x00}
colors[10] = {0xFF, 0xCC, 0x00}
colors[11] = {0xFF, 0xFF, 0x00}
colors[12] = {0xCC, 0xFF, 0x00}
colors[13] = {0x99, 0xFF, 0x00}
colors[14] = {0x66, 0xFF, 0x00}
colors[15] = {0x33, 0xFF, 0x00}
colors[16] = {0x00, 0xFF, 0x00}
colors[17] = {0x00, 0xFF, 0x33}
colors[18] = {0x00, 0xFF, 0x66}
colors[19] = {0x00, 0xFF, 0x99}
colors[20] = {0x00, 0xFF, 0xCC}
colors[21] = {0x00, 0xFF, 0xFF}
colors[22] = {0x00, 0xCC, 0xFF}
colors[23] = {0x00, 0x99, 0xFF}

function Enemy.new()
	local bufX = 5
	local bufY = 5
	local e = {}
	e.loc = {}
	local locIsOk = false
	-- make sure we arent right next to the player
	repeat
		--e.loc.x = (bufX / 2) + (math.random() * (canvas.maxX - bufX * 2))
		e.loc.x = 0
		e.loc.y = bufY + (bufY / 2) + (math.random() * (canvas.maxY - bufY * 3))
		if math.abs(e.loc.x - player.loc.x) > 40 or math.abs(e.loc.y - player.loc.y) > 40 then
			locIsOk = true
		end
	until locIsOk
	e.ring = 1
	e.size = 3 + math.random() * 10
	e.rot = math.random() * 2 * math.pi
	e.rotSpeed = 0.5 + math.random() * 2
	if math.random() < 0.5 then
		e.rotSpeed = e.rotSpeed * -1
	end
	e.alive = true
	e.color = colors[1 + math.floor(math.random() * 23)]
	return e
end

function math.sign(x)
	if x >= 0 then
		return 1
	end
	if x < 0 then
		return -1
	end
end

function love.load()
	math.randomseed(os.time())
	g.setBackgroundColor(255, 255, 255);
	g.clear();
	g.setColor(0, 0, 0);
	canvas.maxX = g.getWidth() / 2
	canvas.maxY = g.getHeight() / 2
end

function drawRing(color, alpha, radius)
	if alpha <= 0 then
		return
	end
	g.setColor(color[1], color[2], color[3], alpha * 200)
	g.circle("line", 0, 0, radius, 100)
end

function love.draw()
	-- help info
	if help > 0 then
		g.push()
		g.scale(1, 1)
		g.translate(4, 10)
		g.setColor(0, 0, 0)
		g.print("**** HOW TO PLAY ***", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("[space] - show/hide help", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("[left] - increase ccw speed", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("[right] - increase cw speed", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("[up] - move away from center", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("[down] - move toward from center", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("r - show player orbit (can be held)", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("s - spawn a ball (rate limited)", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("w - increase spawn rate", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("v - decrease spawn rate", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print("d - show debug info", 0, 0)
		g.pop()
	end

	-- draw debug info if enable
	if debug > 0 then
		g.push()
		g.scale(1, 1)
		g.translate(4, canvas.maxY * 3 / 2)
		g.setColor(0, 0, 0)
		g.print("Debug information", 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print(string.format("spawnLast: %d", spawnLast), 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print(string.format("lastDt: %f", lastDt), 0, 0)
		g.translate(0, g.getFont():getHeight() + 2)
		g.print(string.format("player.rot: %f", tostring(player.rot)), 0, 0)
		g.pop()
	end

	-- reorient things
	g.translate(canvas.maxX, canvas.maxY)
	g.scale(-1, -1)

	g.setColor(255, 0, 0)
	g.circle("fill", 0, 0, 4)

	g.setColor(255, 255, 255)

	-- draw player
	g.push()
		drawRing(player.color, player.ring, player.loc.y + player.height / 2)
		g.setColor(player.color)
		if player.loc.y < 0 then
			player.loc.y = 0
		end
		if (player.loc.y + player.height) > canvas.maxX then
			player.loc.y = canvas.maxX - player.height
		end
		if (player.loc.y + player.height) > canvas.maxY then
			player.loc.y = canvas.maxY - player.height
		end
		g.rotate(-player.rot) -- negative for ccw rot....
		g.translate(player.loc.x, player.loc.y)
		g.triangle("fill", 0, 0, 0, player.height, math.sign(player.rotSpeed) * 30, 10)
	g.pop()

	-- draw enemies
	-- TODO add wiggle
	local elist = enemies
	while elist ~= nil do
		local e = elist.val
		g.push()
			drawRing(e.color, e.ring, e.loc.y)
			g.setColor(e.color)
			g.rotate(-e.rot)
			g.translate(e.loc.x, e.loc.y)
			g.circle("fill", 0, 0, e.size, 20)
		g.pop()
		elist = elist.next
	end
end

function movePlayerIteration(dt)
	-- movement handling
	if k.isDown("up") then
		player.loc.y = player.loc.y + 5
	end
	if k.isDown("down") then
		player.loc.y = player.loc.y - 5
	end
	local dir = math.sign(player.rotSpeed)
	if k.isDown("left") then
		player.rotSpeed = player.rotSpeed + 1/2
	end
	if k.isDown("right") then
		player.rotSpeed = player.rotSpeed - 1/2
	end
	if k.isDown("r") then
		player.ring = .5
	end
	if player.rotSpeed == 0 then
		player.rotSpeed = dir * -1/2
	end
	if math.abs(player.rotSpeed) > player.maxSpeed then
		player.rotSpeed = player.maxSpeed * math.sign(player.rotSpeed)
	end
	player.rot = math.mod(player.rot + player.rotSpeed * math.pi / 180, 2 * math.pi)
	player.ring = player.ring - dt
end

function love.update(dt)
	movePlayerIteration(dt)
	--player.rotSpeed = player.rotSpeed - math.sign(player.rotSpeed) * dt * 10
	--if math.abs(player.rotSpeed) < 1/2 then
	--	player.rotSpeed = math.sign(player.rotSpeed) * 1/2
	--end

	spawnLast = spawnLast + dt
	if spawnLast > spawnWait or (k.isDown("s") and spawnLast > 1) then
		spawnLast = 0
		e = Enemy.new()
		enemies = {val=e, next=enemies}
	end

	if k.isDown("w") then
		spawnWait = spawnWait - 0.1
	end

	if k.isDown("v") then
		spawnWait = spawnWait + 0.1
	end

	local elist = enemies
	local prev = nil
	while elist ~= nil do
		local e = elist.val
		-- check for intersection of enemy and player
		-- TODO calculuate the rot intersection more accurately
		playerRot = math.mod(player.rot, 2 * math.pi)
		eRot = math.mod(e.rot, 2 * math.pi)
		if playerRot < 0 then
			playerRot = 2 * math.pi + playerRot
		end
		if eRot < 0 then
			eRot = 2 * math.pi + eRot
		end
		if math.abs(eRot - playerRot) < 0.05 and
		   math.abs(e.loc.y - player.loc.y) < (player.height + e.size / 2) then
			e.alive = false
			print("killed", tostring(e))
		end

		-- prune dead enemies
		if not e.alive then
			if prev ~= nil then
				prev.next = elist.next
			end
			if prev == nil then
				enemies = elist.next
			end
			elist = elist.next
			if elist == nil then
				break
			end
			e = elist.val
		end
		e.rot = math.mod(e.rot + e.rotSpeed * math.pi / 180, 2 * math.pi)
		if e.ring > 0 then
			e.ring = e.ring - dt
		end
		prev = elist
		elist = elist.next
	end

	-- debug counters
	lastDt = dt
	if k.isDown("d") then
		if debug < -1.2 then
			debug = 1
		end
		if debug > 1.2 then
			debug = -1
		end
	end
	debug = debug + dt * math.sign(debug)

	if k.isDown(" ") then
		if help < -1.2 then
			help = 1
		end
		if help > 1.2 then
			help = -1
		end
	end
	help = help + dt * math.sign(help)
end

