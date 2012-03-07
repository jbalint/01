
g = love.graphics
i = love.image
k = love.keyboard
m = love.mouse

bgImage = nil
boardImage = nil
bgImageData = nil

curve = nil
curves = {}
movePoint = nil
--mouseMovePoint = nil
mouseBeginPoint = nil

player = {}
player.rot = 0
player.pos = {0, 0}
player.moveCurve = nil
player.drawCurve = nil
player.moveT = 0

BezQuad = {}
function BezQuad:new()
	local bc = {}
	setmetatable(bc, self)
	self.__index = self
	bc.points = {
		{70, 250},
		{20, 110},
		{250, 60}
	}
	return bc
end

function BezQuad:binom(t, c1, c2, c3)
	return math.pow(1-t, 2) * c1 + 2 * (1 - t) * t * c2 + math.pow(t, 2) * c3
end

function BezQuad:draw()
	local t = 0
	local p1 = self.points[1]
	local p2 = self.points[2]
	local p3 = self.points[3]

	g.setColor(128, 0, 128)
	while t <= 1 do
		g.point(self:binom(t, p1[1], p2[1], p3[1]),
				self:binom(t, p1[2], p2[2], p3[2]))
		t = t + 1 / g.getWidth()
	end

	if false then
		g.setColor(0, 0, 0)
		g.circle("line", p1[1], p1[2], 3)
		g.circle("line", p2[1], p2[2], 3)
		g.circle("line", p3[1], p3[2], 3)

		g.line(p1[1], p1[2], p2[1], p2[2])
		g.line(p2[1], p2[2], p3[1], p3[2])
		--g.line(p3[1], p3[2], p1[1], p1[2])
	end
end

BezCube = {}
function BezCube:new()
	local bc = {}
	setmetatable(bc, self)
	self.__index = self
	bc.points = {
		{150 + 120, 160},
		{150 + 35, 200},
		{150 + 220, 260},
		{150 + 220, 40}
	}
	return bc
end

function BezCube:binom(t, c1, c2, c3, c4)
	return math.pow(1 - t, 3) * c1 + 3 * math.pow(1 - t, 2) * t * c2 +
		3 * (1 - t) * math.pow(t, 2) * c3 + math.pow(t, 3) * c4
end

function BezCube:draw()
	local t = 0
	local p1 = self.points[1]
	local p2 = self.points[2]
	local p3 = self.points[3]
	local p4 = self.points[4]

	g.setColor(128, 128, 255)
	while t <= 1 do
		g.point(self:binom(t, p1[1], p2[1], p3[1], p4[1]),
				self:binom(t, p1[2], p2[2], p3[2], p4[2]))
		t = t + 1 / g.getWidth()
	end

	g.setColor(0, 0, 0)
	g.circle("line", p1[1], p1[2], 3)
	g.circle("line", p2[1], p2[2], 3)
	g.circle("line", p3[1], p3[2], 3)
	g.circle("line", p4[1], p4[2], 3)

	g.line(p1[1], p1[2], p2[1], p2[2])
	--g.line(p2[1], p2[2], p3[1], p3[2])
	g.line(p3[1], p3[2], p4[1], p4[2])
	--g.line(p4[1], p4[2], p1[1], p1[2])
end

function love.load()
	bgImage = g.newImage("lvl1.png")
	boardImage = g.newImage("lvl1_layout.png")
	bgImageData = i.newImageData("lvl1_layout.png")
	--bgImageData = i.newImageData("lvl1.png")
	player.image = g.newImage("Resource-HamsterBall.png")
	--player.pos[1] = player.pos[1] + player.image:getWidth() / 2
	--player.pos[2] = player.pos[2] + player.image:getHeight() / 2
	g.setBackgroundColor(255, 255, 255)
	g.clear()
	g.setColor(0, 0, 0)

	--table.insert(curves, BezQuad:new())
	--table.insert(curves, BezCube:new())

	print("width")
	print(g.getWidth())
	print("height")
	print(g.getHeight())
end

function love.draw()
	g.setColorMode("replace")
	g.draw(bgImage, 0, 0)
	g.draw(boardImage, 0, 0, 0, 20, 20)
	if player.pos[1] < 0 then
		player.pos[1] = 0
	end
	if player.drawCurve ~= nil then
		player.drawCurve:draw()
	end
	if player.moveCurve ~= nil then
		player.moveCurve:draw()
	end
	g.draw(player.image, player.pos[1], player.pos[2], player.rot)
	for i, v in ipairs(curves) do
		v:draw()
	end
end

function love.update(dt)
	if player.drawCurve ~= nil then
		player.drawCurve.points[3][1] = m.getX()
		player.drawCurve.points[3][2] = m.getY()
	end
	if player.drawCurve == nil and player.moveCurve == nil then
		if k.isDown("left") then
			player.pos[1] = player.pos[1] - 3
		elseif k.isDown("right") then
			player.pos[1] = player.pos[1] + 3
		end
	end
	if player.moveCurve ~= nil then
		local p1 = player.moveCurve.points[1]
		local p2 = player.moveCurve.points[2]
		local p3 = player.moveCurve.points[3]
		local newX = player.moveCurve:binom(player.moveT, p1[1], p2[1], p3[1]) - player.image:getWidth() / 2
		local newY = player.moveCurve:binom(player.moveT, p1[2], p2[2], p3[2])
				local pTl = {newX, newY}
				local pTr = {newX + player.image:getWidth(), newY}
				local pBl = {newX, newY + player.image:getHeight()}
				local pBr = {newX + player.image:getWidth(), newY + player.image:getHeight()}
				local corners = {pTl, pTr, pBl, pBr}
				--local canMove = true
				for i, v in ipairs(corners) do
					local pr, pg, pb, pa = bgImageData:getPixel(math.max(0, math.floor(v[1] / 20)), math.max(0, math.floor(v[2] / 20)))
					if pa ~= 0 then
						player.moveCurve = nil
						break
					end
				end
		if player.moveCurve ~= nil then
			player.pos[1] = newX
			player.pos[2] = newY
			player.moveT = player.moveT + 3/100
			if player.moveT >= 1 then
				player.moveCurve = nil
			end
		end
	end
	if player.moveCurve == nil then
		--while bgImageData:getPixel(player.pos[1], player.pos[2] + height)[1] == 255 do
			local i = 5
			while i >= 0 do
				local pTl = {player.pos[1], player.pos[2] + 1}
				local pTr = {player.pos[1] + player.image:getWidth(), player.pos[2] + 1}
				local pBl = {player.pos[1], player.pos[2] + player.image:getHeight() + 1}
				local pBr = {player.pos[1] + player.image:getWidth(), player.pos[2] + player.image:getHeight() + 1}
				local corners = {pTl, pTr, pBl, pBr}
				local canMove = true
				for i, v in ipairs(corners) do
					--if pa ~= 0 then
					--print (string.format("px %d, %d", v[1], v[2]))
					--print (string.format("px %d, %d", math.floor(v[1] / 20), math.floor(v[2] / 20)))
					--end
					local pr, pg, pb, pa = bgImageData:getPixel(math.max(0, math.floor(v[1] / 20)), math.max(0, math.floor(v[2] / 20)))
					--	print (string.format("pa: %d", pa))
					--if pa == 0 then
					--	print (string.format("pa: %d", pa))
					--end
					if pa ~= 0 then
						canMove = false
						break
					end
				end
				if canMove and player.pos[2] + player.image:getHeight() + 1 < g.getHeight() then
					player.pos[2] = player.pos[2] + 1
				end
				i = i - 1
			end
		--end
	end
	--player.rot = player.rot + 1
	--if m.isDown("l") and mouseMovePoint ~= nil then
	--	mouseMovePoint[1] = m.getX()
	--	mouseMovePoint[2] = m.getY()
	--end
	if movePoint ~= nil then
		-- perform actual movement of point if mouse is clicked
		if k.isDown("left") then
			movePoint[1] = movePoint[1] - 8
		end
		if k.isDown("right") then
			movePoint[1] = movePoint[1] + 8
		end
		if k.isDown("up") then
			movePoint[2] = movePoint[2] - 8
		end
		if k.isDown("down") then
			movePoint[2] = movePoint[2] + 8
		end
	end
end

function distance(p1, p2)
	return math.abs(math.sqrt(math.pow(p2[1] - p1[1], 2) + math.pow(p2[2] - p1[2], 2)))
end

function love.mousepressed(x, y, button)
	if player.moveCurve ~= nil then
		return
	end
	local mousePoint = {x, y}
	player.drawCurve = BezQuad:new()
	player.drawCurve.points[1][1] = player.pos[1] + player.image:getWidth() / 2
	player.drawCurve.points[1][2] = player.pos[2]
	player.drawCurve.points[2][1] = player.pos[1] + player.image:getWidth() / 2
	player.drawCurve.points[2][2] = player.pos[2]
	player.moveT = 0
	movePoint = player.drawCurve.points[2]
	-- left click allows to move a point on a curve
	--if button == "l" then
	--	for c_i, curve in ipairs(curves) do
	--		-- check if we're near any curve point and move it
	--		for i, v in ipairs(curve.points) do
	--			if distance(v, mousePoint) <= 5 then
	--				mouseBeginPoint = mousePoint
	--				mouseMovePoint = v
	--				m.setPosition(unpack(v))
	--				m.setGrab(true)
	--				break
	--			end
	--		end
	--		if mouseMovePoint ~= nil then
	--			break
	--		end
	--	end
	--end
end

function love.mousereleased(x, y, button)
	if button == "l" and player.drawCurve ~= nil then
		player.moveCurve = player.drawCurve
		player.drawCurve = nil
		--mouseMovePoint = nil
		m.setGrab(false)
	end
end

function love.keypressed(key)
	---- ignore key presses when mouse is down
	----if m.isDown("l") then
	----	return
	----end
	--local keyPoint = nil
	--if key == "z" then
	--	keyPoint = curves[1].points[1]
	--elseif key == "x" then
	--	keyPoint = curves[1].points[2]
	--elseif key == "c" then
	--	keyPoint = curves[1].points[3]
	--elseif key == "a" then
	--	keyPoint = curves[2].points[1]
	--elseif key == "s" then
	--	keyPoint = curves[2].points[2]
	--elseif key == "d" then
	--	keyPoint = curves[2].points[3]
	--elseif key == "f" then
	--	keyPoint = curves[2].points[4]
	--end

	---- only do this if a key was hit that refers to a point
	--if keyPoint then
	--	-- toggle moving off if same key
	--	if movePoint == keyPoint then
	--		movePoint = nil
	--	-- else set new move point
	--	else
	--		movePoint = keyPoint
	--	end
	--end
end

