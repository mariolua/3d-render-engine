
local vector = require('vector')
local bit = require('bit')
local ffi = require('ffi')

math.randomseed(globals.realtime())

-- Cache frequently used string functions for performance
local string_match, string_len, string_gsub, string_gmatch, string_byte = 
    string.match, string.len, string.gsub, string.gmatch, string.byte
    

-- Utilize local functions for math operations to reduce global lookup overhead
local math_cos, math_sin, math_rad, math_sqrt, math_min, math_max, math_floor, math_huge, math_pi, table_insert =
    math.cos, math.sin, math.rad, math.sqrt, math.min, math.max, math.floor, math.huge, math.pi, table.insert

-- Precompute constants outside of functions to avoid recalculating them
local rad_to_deg = math.pi / 180

-- Cache frequently used ffi functions for performance
local cast, typeof, ffi_string = ffi.cast, ffi.typeof, ffi.string

-- Search for required signatures in the engine.dll
local pGetModuleHandle_sig = client.find_signature('engine.dll', '\xFF\x15\xCC\xCC\xCC\xCC\x85\xC0\x74\x0B') 
    or error('Couldn\'t find signature #1')

local pGetProcAddress_sig = client.find_signature('engine.dll', '\xFF\x15\xCC\xCC\xCC\xCC\xA3\xCC\xCC\xCC\xCC\xEB\x05') 
    or error('Couldn\'t find signature #2')

local jmp_ecx = client.find_signature('engine.dll', '\xFF\xE1')

-- Extract actual function addresses from the signatures
local pGetProcAddress = cast('uint32_t**', cast('uint32_t', pGetProcAddress_sig) + 2)[0][0]
local fnGetProcAddress = cast('uint32_t(__fastcall*)(unsigned int, unsigned int, uint32_t, const char*)', jmp_ecx)

local pGetModuleHandle = cast('uint32_t**', cast('uint32_t', pGetModuleHandle_sig) + 2)[0][0]
local fnGetModuleHandle = cast('uint32_t(__fastcall*)(unsigned int, unsigned int, const char*)', jmp_ecx)

--- Binds a given function from a module and returns a callable Lua function for it.
-- @param module_name The name of the module containing the function.
-- @param function_name The name of the function to bind.
-- @param typedef The FFI typedef string defining the function's signature.
-- @return A Lua function that can be called to invoke the bound function.
local function proc_bind(module_name, function_name, typedef)
    local ctype = typeof(typedef)
    local module_handle = fnGetModuleHandle(pGetModuleHandle, 0, module_name)
    local proc_address = fnGetProcAddress(pGetProcAddress, 0, module_handle, function_name)
    local call_fn = cast(ctype, jmp_ecx)

    return function(...)
        return call_fn(proc_address, 0, ...) -- Always pass two dummy arguments
    end
end

-- Define the typedef for SetCursorPos and GetCursorPos
local CursorPos_typedef = [[
    typedef int BOOL;
    typedef struct {
        long x;
        long y;
    } POINT, *LPPOINT;
    BOOL GetCursorPos(LPPOINT lpPoint);
    BOOL SetCursorPos(int X, int Y);
    BOOL ShowCursor(BOOL bShow);
]]
ffi.cdef(CursorPos_typedef)

-- Bind the SetCursorPos and GetCursorPos functions for future use
local SetCursorPos = proc_bind('user32.dll', 'SetCursorPos', 'int32_t(__fastcall*)(uint32_t, uint32_t, int, int)')
local GetCursorPos = proc_bind('user32.dll', 'GetCursorPos', 'int32_t(__fastcall*)(uint32_t, uint32_t, LPPOINT)')

-- Bind the ShowCursor function
local ShowCursor = proc_bind('user32.dll', 'ShowCursor', 'int32_t(__fastcall*)(uint32_t, uint32_t, int)')


-- Function to hide the cursor
local function hideCursor()
    local val = ShowCursor(0)
    if val ~= 0 then
        local arg = val <= 0 and 0 or 1
        for i=1, math.abs(val) do
            ShowCursor(arg)
        end
    end
end

-- Function to show the cursor
local function showCursor()
    local val = ShowCursor(1)
    if val ~= 1 then
        local arg = val < 1 and 1 or 0
        for i=1, math.abs(val) do
            ShowCursor(arg)
        end
    end
end

-- Find the signature in `engine.dll` related to the window handle.
local raw_hwnd = client.find_signature('engine.dll', '\x8B\x0D\xCC\xCC\xCC\xCC\x85\xC9\x74\x16\x8B\x01\x8B')

-- Find the signature in `gameoverlayrenderer.dll` related to jump instruction.
local raw_insn_jmp_ecx = client.find_signature('gameoverlayrenderer.dll', '\xFF\xE1')

-- Find the signature in `gameoverlayrenderer.dll` related to getting the foreground window.
local raw_GetForegroundWindow = client.find_signature('gameoverlayrenderer.dll', '\xFF\x15\xCC\xCC\xCC\xCC\x3B\xC6\x74')

-- Convert the raw window handle signature to a usable pointer.
local hwnd_ptr = ((ffi.cast('uintptr_t***', ffi.cast('uintptr_t', raw_hwnd) + 2)[0])[0] + 2)

-- Convert the jump instruction signature to a callable function.
local insn_jmp_ecx = ffi.cast('int(__thiscall*)(uintptr_t)', raw_insn_jmp_ecx)

-- Convert the get foreground window signature to a usable pointer.
local GetForegroundWindow = (ffi.cast('uintptr_t**', ffi.cast('uintptr_t', raw_GetForegroundWindow) + 2)[0])[0]

-- Cache for the CS:GO window handle. 
-- This is used to avoid unnecessary calls to GetCSGOHwnd if the value is already retrieved once.
local csgo_hwnd_cache

-- Function to get CS:GO's window handle.
local function GetCSGOHwnd()
	return hwnd_ptr[0]
end

-- Function to get the handle of the current foreground window.
local function GetForegroundHwnd()
	return insn_jmp_ecx(GetForegroundWindow)
end

-- Function to check if the CS:GO window is currently in the foreground.
local function isInWindow()
	local csgo_hwnd = csgo_hwnd_cache or GetCSGOHwnd() -- Use cached value if exists, otherwise fetch.
	csgo_hwnd_cache = csgo_hwnd -- Update cache.
	local foreground_hwnd = GetForegroundHwnd()
	return (csgo_hwnd == foreground_hwnd)
end

local screenWidth, screenHeight = client.screen_size()
local screenCenterX = screenWidth / 2
local screenCenterY = screenHeight / 2
local sensitivity = 0.1  -- Adjust based on how fast you want the camera to rotate
local lightCircleRadius = 100  -- Adjust this value for the size of the light circle
local lightAngle = 0  -- Start at 0 degrees
local lightPosition = vector(0, 0, 0)  -- Initialize the light position at the origin
local lightDirection = vector(0, 0, 0)
local lightRadius = 30  -- This is the radius of the circle representing the light source on the screen. Adjust as needed.
local lightScreenX, lightScreenY = 0, 0

local camera = {
    pos = vector(0, 0, 20),
    yaw = 0,
    pitch = 0,
    roll = 0,
    speed = 0.1,
    length = 1,
    width = 1,
    height = 5,
    yVelocity = 0,
    isGrounded = true,
    jumpForce = 2,
    gravity = -0.08,
    velocity = vector(0, 0, 0),
    airControl = 0.7,  -- Controls how much influence the player has over movement while in the air (0 is none, 1 is full control).
    terminalVelocity = -1.5,  -- Maximum falling speed.
    friction = 0.8, -- The lower the more friction. sounds logical, right?
    inAirFriction = 0.95,
    radius = 0.5
}
camera.pos.y = camera.pos.y + camera.height

local key_codes = { 
    space = 0x20,
    m1 = 0x01,      -- Left mouse button
    m2 = 0x02,      -- Right mouse button
    m3 = 0x04,      -- Middle mouse button (three-button mouse)
    m4 = 0x05,      -- X1 mouse button
    m5 = 0x06,      -- X2 mouse button
    w = 0x57,
    a = 0x41,
    s = 0x53,
    d = 0x44,
    q = 0x51,
    e = 0x45,
    f = 0x46,
    r = 0x52,
    z = 0x5A,
    x = 0x58,
    c = 0x43,
    v = 0x56,
    b = 0x42,
    n = 0x4E,
    m = 0x4D,
    lshift = 0xA0,  -- Left SHIFT key
    rshift = 0xA1,  -- Right SHIFT key
    lctrl = 0xA2,   -- Left CONTROL key
    rctrl = 0xA3,   -- Right CONTROL key
    lalt = 0xA4,    -- Left MENU key (Alt key)
    ralt = 0xA5,    -- Right MENU key (Alt key)
    tab = 0x09,
    enter = 0x0D,
    esc = 0x1B,
    up = 0x26,      -- Up arrow key
    down = 0x28,    -- Down arrow key
    left = 0x25,    -- Left arrow key
    right = 0x27,   -- Right arrow key
}

local fov = 95 -- in degrees
local aspectRatio = (screenWidth + 100) / (screenHeight + 56.24999999999999)
local near = 0
local fovRad = 1 / math.tan(fov * 0.5 / 180 * math.pi)
local actualFovRad = fov * math.pi / 180
local fov_slider = ui.new_slider('LUA', 'A', 'Fov', 1, 180, fov, true, '°')

ui.set_callback(fov_slider, function(obj)
    fov = ui.get(obj)
    aspectRatio = (screenWidth + 100) / (screenHeight + 56.24999999999999)
    fovRad = 1 / math.tan(fov * 0.5 / 180 * math.pi)
    actualFovRad = fov * math.pi / 180
end)

local function math_clamp(val, min, max)
    return math_max(min, math_min(max, val))
end

local function math_round(num, decimals)
	num = num or 0
	local mult = 10 ^ (decimals or 0)
	return math_floor(num * mult + 0.5) / mult
end

local function math_percent(percent, maxvalue)
	if tonumber(percent) and tonumber(maxvalue) then
		return (maxvalue * percent) / 100
	end
	return false
end

local function math_percentof(cur_value, maxvalue)
	cur_value = cur_value == 0 and 1 or cur_value
	maxvalue = maxvalue == 0 and 1 or maxvalue
	if tonumber(cur_value) and tonumber(maxvalue) then
		return 100 - (( (maxvalue - cur_value) / maxvalue ) * 100)
	end
	return false
end

local function crossProduct(v1, v2)
    local result = vector(
        v1.y * v2.z - v1.z * v2.y,
        v1.z * v2.x - v1.x * v2.z,
        v1.x * v2.y - v1.y * v2.x
    )
    return result
end


local function project(x, y, z)
    local vec = (x and not y) and x or vector(x, y, z)
    
    -- Translate the world relative to the camera
    local d_vec = vec - camera.pos

    -- Rotate around y-axis using the yaw
    local cosYaw = math_cos(math_rad(camera.yaw))
    local sinYaw = math_sin(math_rad(camera.yaw))
    
    local tmpX = d_vec.x * cosYaw - d_vec.z * sinYaw
    local tmpZ = d_vec.x * sinYaw + d_vec.z * cosYaw

    -- Rotate around x-axis using the pitch
    local cos_pitch = math_cos(math_rad(camera.pitch))
    local sin_pitch = math_sin(math_rad(camera.pitch))
    
    local rotatedX = tmpX
    local rotatedY = d_vec.y * cos_pitch - tmpZ * sin_pitch
    local rotatedZ = d_vec.y * sin_pitch + tmpZ * cos_pitch
    
    -- Check if the point is behind the camera's near clipping plane
    if rotatedZ >= near then
        return nil, nil
    end
    
    -- Perspective projection
    local px = rotatedX * (fovRad / aspectRatio) / rotatedZ
    local py = rotatedY * fovRad / rotatedZ

    -- Convert to screen space
    local xProjected = (px + 1) * 0.5 * screenWidth
    local yProjected = (py + 1) * 0.5 * screenHeight

    return xProjected, yProjected
end

-- Combine common logic in isVertexInFOV and isSphereInFOV functions
local function isObjectProjectedOnScreen(xProjected, yProjected)
    return xProjected and yProjected and xProjected >= 0 and xProjected <= screenWidth and yProjected >= 0 and yProjected <= screenHeight
end

-- Refactor isVertexInFOV and isSphereInFOV to use the common function
local function isVertexInFOV(x, y, z)
    local xProjected, yProjected = project(x, y, z)
    return isObjectProjectedOnScreen(xProjected, yProjected)
end

local function isSphereInFOV(x, y, z, radius)
    -- Project the center of the sphere onto the 2D screen space
    local xProjected, yProjected = project(x, y, z)

    -- If the center is not projected (e.g., behind the camera), the sphere is not in FOV
    if not xProjected or not yProjected then
        return false
    end
    
    -- Calculate the projected radius
    -- Assuming a simple linear relationship, which might not be perfect but should work for this purpose
    local distanceToCamera = math.sqrt((x - camera.pos.x)^2 + (y - camera.pos.y)^2 + (z - camera.pos.z)^2)
    local projectedRadius = (radius / distanceToCamera) * screenCenterX  -- Using screenCenterX as an approximation for projection
    
    -- Check if the sphere is within the screen's boundaries or touching the boundaries
    if (xProjected + projectedRadius >= 0 and xProjected - projectedRadius <= screenWidth) and
       (yProjected + projectedRadius >= 0 and yProjected - projectedRadius <= screenHeight) then
        return true  -- The sphere is at least partially visible
    end
    
    return false  -- The sphere is outside the camera's FOV
end

local function isSphereInView(center, radius)
    -- Project the bounding box of the sphere
    for dx = -1, 1, 2 do
        for dy = -1, 1, 2 do
            for dz = -1, 1, 2 do
                local xProjected, yProjected = project(center.x + dx * radius, center.y + dy * radius, center.z + dz * radius)
                if xProjected and yProjected and xProjected >= 0 and xProjected <= screenWidth and yProjected >= 0 and yProjected <= screenHeight then
                    return true  -- At least a part of the sphere's bounding box is in view
                end
            end
        end
    end
    return false  -- Bounding box entirely outside the FOV
end

local function isObjectInFOV(x, y, z, width, height, length)
    local radius = (width and not height and not length) and width or nil

    if radius then
        return isSphereInView(vector(x, y, z), radius)
    elseif x and y and z and not width then
        return isVertexInFOV(x, y, z)
    else
        -- Calculate half dimensions
        local halfWidth = width / 2
        local halfHeight = height / 2
        local halfLength = length / 2
        
        -- Check each vertex of the object
        for _, vertex in ipairs({
            {x - halfWidth, y - halfHeight, z - halfLength},
            {x + halfWidth, y - halfHeight, z - halfLength},
            {x - halfWidth, y + halfHeight, z - halfLength},
            {x + halfWidth, y + halfHeight, z - halfLength},
            {x - halfWidth, y - halfHeight, z + halfLength},
            {x + halfWidth, y - halfHeight, z + halfLength},
            {x - halfWidth, y + halfHeight, z + halfLength},
            {x + halfWidth, y + halfHeight, z + halfLength}
        }) do
            if isVertexInFOV(vertex[1], vertex[2], vertex[3]) then
                return true  -- At least one vertex is visible, hence the object is partially in FOV
            end
        end
        
        return false  -- No vertices are in the FOV, hence the object is entirely outside the FOV
    end

end

local native_Surface_DrawSetColor = vtable_bind('vguimatsurface.dll', 'VGUI_Surface031', 15, 'void(__thiscall*)(void*, int, int, int, int)')
local native_Surface_DrawFilledRect = vtable_bind('vguimatsurface.dll', 'VGUI_Surface031', 16, 'void(__thiscall*)(void*, int, int, int, int)')
local native_Surface_DrawOutlinedRect = vtable_bind('vguimatsurface.dll', 'VGUI_Surface031', 18, 'void(__thiscall*)(void*, int, int, int, int)')
local native_Surface_DrawLine = vtable_bind('vguimatsurface.dll', 'VGUI_Surface031', 19, 'void(__thiscall*)(void*, int, int, int, int)')
local native_Surface_DrawPolyLine = vtable_bind('vguimatsurface.dll', 'VGUI_Surface031', 20, 'void(__thiscall*)(void*, int*, int*, int)')
local native_Surface_DrawOutlinedCircle = vtable_bind('vguimatsurface.dll', 'VGUI_Surface031', 103, 'void(__thiscall*)(void*, int, int, int, int)')
local native_Surface_DrawFilledRectFade = vtable_bind('vguimatsurface.dll', 'VGUI_Surface031', 123, 'void(__thiscall*)(void*, int, int, int, int, unsigned int, unsigned int, bool)')

local function renderer_filled_gradient_rect(x, y, w, h, r0, g0, b0, a0, r1, g1, b1, a1, horizontal)
	native_Surface_DrawSetColor(r0, g0, b0, a0)
	native_Surface_DrawFilledRectFade(x, y, x + w, y + h, 255, 255, horizontal)

	native_Surface_DrawSetColor(r1, g1, b1, a1)
	return native_Surface_DrawFilledRectFade(x, y, x + w, y + h, 0, 255, horizontal)
end

local function renderer_filled_rect(x, y, w, h, r, g, b, a)
	native_Surface_DrawSetColor(r, g, b, a)
	return native_Surface_DrawFilledRect(x, y, x + w, y + h)
end

local function renderer_poly_line(x, y, r, g, b, a, count)
	native_Surface_DrawSetColor(r, g, b, a)
	return native_Surface_DrawPolyLine(new_intptr(x), new_intptr(y), count)
end

local function pointInPolygon(p, vertices)
    local numVertices = #vertices
    local i, j = numVertices, 1
    local c = false
    for i = 1, numVertices do
        if ((vertices[i][2] > p[2]) ~= (vertices[j][2] > p[2])) and
           (p[1] < (vertices[j][1] - vertices[i][1]) * (p[2] - vertices[i][2]) / (vertices[j][2] - vertices[i][2]) + vertices[i][1]) then
            c = not c
        end
        j = i
    end
    return c
end

local function renderer_filled_rgba_polygon(vertices, r, g, b, a)
    local numVertices = #vertices
    if numVertices < 3 then
        print('Error: A polygon requires at least 3 vertices.')
        return
    end

    local minX, minY, maxX, maxY = math_huge, math_huge, -math_huge, -math_huge
    for _, vertex in ipairs(vertices) do
        minX = math_min(minX, vertex[1])
        minY = math_min(minY, vertex[2])
        maxX = math_max(maxX, vertex[1])
        maxY = math_max(maxY, vertex[2])
    end

    local width = maxX - minX + 1
    local height = maxY - minY + 1

    local buffer = {}
    for y = minY, maxY do
        for x = minX, maxX do
            if pointInPolygon({x, y}, vertices) then
                table_insert(buffer, string.char(r, g, b, a))
            else
                table_insert(buffer, string.char(0, 0, 0, 0))  -- Transparent pixel
            end
        end
    end

    local rgba_contents = table.concat(buffer)
    local rgba_texture = renderer.load_rgba(rgba_contents, width, height)

    renderer.texture(rgba_texture, minX, minY, width, height, 255, 255, 255, 255, 'f')
end

local function renderer_polygon(vertices, r, g, b, a)
    local numVertices = #vertices
    if numVertices < 3 then
        print('Error: A polygon requires at least 3 vertices.')
        return
    end
    
    for i = 1, numVertices do
        local x1, y1 = vertices[i][1], vertices[i][2]
        local x2, y2
        
        -- If it's the last vertex, connect it with the first one to close the polygon.
        if i == numVertices then
            x2, y2 = vertices[1][1], vertices[1][2]
        else
            x2, y2 = vertices[i+1][1], vertices[i+1][2]
        end
        
        renderer.line(x1, y1, x2, y2, r, g, b, a)
    end
end

local function renderer_filled_polygon(vertices, r, g, b, a)
    local numVertices = #vertices
    if numVertices < 3 then
        print('Error: A polygon requires at least 3 vertices.')
        return
    end

    -- Find the bounding box of the polygon
    local minY = vertices[1][2]
    local maxY = minY

    for i = 2, numVertices do
        minY = math_min(minY, vertices[i][2])
        maxY = math_max(maxY, vertices[i][2])
    end

    -- Process each scanline
    for y = minY, maxY do
        local intersections = {}

        -- Find intersections of the scanline with all edges of the polygon
        for i = 1, numVertices do
            local x1, y1 = vertices[i][1], vertices[i][2]
            local x2, y2 = vertices[(i % numVertices) + 1][1], vertices[(i % numVertices) + 1][2]

            if y1 < y2 then
                x1, y1, x2, y2 = x2, y2, x1, y1  -- Swap ends
            end

            if y >= y1 and y < y2 then
                local xi = x1 + (x2 - x1) * (y - y1) / (y2 - y1)
                table_insert(intersections, xi)
            end
        end

        -- Sort intersections by x-coordinates
        table.sort(intersections)

        -- Fill between pairs of intersections
        for i = 1, #intersections - 1, 2 do
            for x = intersections[i], intersections[i + 1] do
                print('Filling pixel at:', x, y)
                renderer_filled_rect(x, y, 2, 2, r, g, b, a)
            end
        end
    end
end

local function pointInTriangle(px, py, ax, ay, bx, by, cx, cy)
    -- Create vectors for the points and triangle vertices
    local point = vector(px, py, 0)
    local vertexA = vector(ax, ay, 0)
    local vertexB = vector(bx, by, 0)
    local vertexC = vector(cx, cy, 0)

    -- Compute vectors from point A to point B and point C
    local v0 = vertexC - vertexA
    local v1 = vertexB - vertexA
    local v2 = point - vertexA

    -- Compute dot products
    local dot00 = v0:dot(v0)
    local dot01 = v0:dot(v1)
    local dot02 = v0:dot(v2)
    local dot11 = v1:dot(v1)
    local dot12 = v1:dot(v2)

    -- Compute barycentric coordinates
    local invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
    local u = (dot11 * dot02 - dot01 * dot12) * invDenom
    local v = (dot00 * dot12 - dot01 * dot02) * invDenom

    -- Check if point is in triangle
    return (u >= 0) and (v >= 0) and (u + v < 1)
end


local function isEar(p1, p2, p3, vertices)
    local vecP1 = vector(p1[1], p1[2], 0)
    local vecP2 = vector(p2[1], p2[2], 0)
    local vecP3 = vector(p3[1], p3[2], 0)

    -- Check triangle orientation using cross product
    local area = (vecP2 - vecP1):cross(vecP3 - vecP1).z
    if area <= 0 then
        return false
    end

    -- Check if any vertex is inside the triangle formed by p1, p2, and p3
    for _, vertex in ipairs(vertices) do
        local vecVertex = Vector(vertex[1], vertex[2], 0)
        if vecVertex ~= vecP1 and vecVertex ~= vecP2 and vecVertex ~= vecP3 and pointInTriangle(vertex[1], vertex[2], p1[1], p1[2], p2[1], p2[2], p3[1], p3[2]) then
            return false
        end
    end
    return true
end

local function filled_polygon(vertices, r, g, b, a)
    local numVertices = #vertices
    if numVertices < 3 then return end

    local active = {}
    for i = 1, numVertices do active[i] = true end

    local function findNextActiveIndex(index)
        local nextIndex = index % numVertices + 1
        while not active[nextIndex] and nextIndex ~= index do
            nextIndex = nextIndex % numVertices + 1
        end
        return nextIndex
    end

    local i = 1
    while numVertices > 2 do
        local next = findNextActiveIndex(i)
        local nextNext = findNextActiveIndex(next)

        local p1, p2, p3 = vertices[i], vertices[next], vertices[nextNext]

        if isEar(p1, p2, p3, vertices) then
            renderer.triangle(p1[1], p1[2], p2[1], p2[2], p3[1], p3[2], r, g, b, a)
            active[next] = false
            numVertices = numVertices - 1
        end

        i = findNextActiveIndex(i)
    end
end

local function rounded_rectangle(x, y, width, height, r, g, b, a, rounding)
    rounding = math_clamp(rounding, 0, math_min(width, height) / 2)

    renderer.rectangle(x, y + rounding, width, height - rounding * 2, r, g, b, a)
    renderer.rectangle(x + rounding, y, width - (rounding * 2), rounding, r, g, b, a)
    renderer.rectangle(x + rounding, y + height - rounding, width - (rounding * 2), rounding, r, g, b, a)

    renderer.circle(x + rounding, y + rounding, r, g, b, a, rounding, 180, 0.25)
    renderer.circle(x + width - rounding, y + rounding, r, g, b, a, rounding, 90, 0.25)
    renderer.circle(x + width - rounding, y + height - rounding, r, g, b, a, rounding, 0, 0.25)
    renderer.circle(x + rounding, y + height - rounding, r, g, b, a, rounding, 270, 0.25)
end

local function interpolate(start, stop, stepNumber, maxStep)
    if maxStep == 0 then
        return start
    else
        return start + (stop - start) * (stepNumber / maxStep)
    end
end

function renderer.gradientTriangle2(v1, v2, v3, c1, c2, c3)
    local vertices = {{v1, c1}, {v2, c2}, {v3, c3}}
    table.sort(vertices, function(a, b) return a[1][2] < b[1][2] end)

    local function handlePart(startVert, endVert, startY, endY, isTop)
        for y = startY, endY, 1 do
            local alpha = (endVert[1][2] - startVert[1][2]) ~= 0 and (y - startVert[1][2]) / (endVert[1][2] - startVert[1][2]) or 0
            local xStart = interpolate(startVert[1][1], endVert[1][1], alpha, 1)
            local xEnd = interpolate(isTop and vertices[1][1][1] or vertices[3][1][1], endVert[1][1], alpha, 1)

            local colorStart = {interpolate(startVert[2][1], endVert[2][1], alpha, 1), interpolate(startVert[2][2], endVert[2][2], alpha, 1), interpolate(startVert[2][3], endVert[2][3], alpha, 1)}
            local colorEnd = {interpolate(isTop and vertices[1][2][1] or vertices[3][2][1], endVert[2][1], alpha, 1), interpolate(isTop and vertices[1][2][2] or vertices[3][2][2], endVert[2][2], alpha, 1), interpolate(isTop and vertices[1][2][3] or vertices[3][2][3], endVert[2][3], alpha, 1)}

            renderer.gradient(xStart, y, xEnd - xStart, 1, unpack(colorStart), 255, unpack(colorEnd), 255, true)
        end
    end

    handlePart(vertices[1], vertices[2], vertices[1][1][2], vertices[2][1][2], false) -- Flat bottom
    handlePart(vertices[2], vertices[3], vertices[2][1][2], vertices[3][1][2], true)  -- Flat top
end


function renderer.gradientTriangle(v1, v2, v3, c1, c2, c3)
    -- Sort vertices by y-coordinate
    local vertices = {v1, v2, v3}
    local colors = {c1, c2, c3}
    
    table.sort(vertices, function(a, b) return a[2] < b[2] end)
    table.sort(colors, function(a, b) return a[2] < b[2] end)

    local top = vertices[1]
    local middle = vertices[2]
    local bottom = vertices[3]

    local colorTop = colors[1]
    local colorMiddle = colors[2]
    local colorBottom = colors[3]

    -- Handle the flat bottom triangle
    for y = top[2], middle[2], 1 do
        local alpha = (y - top[2]) / (middle[2] - top[2])

        local xStart = interpolate(top[1], middle[1], y - top[2], middle[2] - top[2])
        local xEnd = interpolate(top[1], bottom[1], y - top[2], bottom[2] - top[2])

        local rStart = interpolate(colorTop[1], colorMiddle[1], y - top[2], middle[2] - top[2])
        local gStart = interpolate(colorTop[2], colorMiddle[2], y - top[2], middle[2] - top[2])
        local bStart = interpolate(colorTop[3], colorMiddle[3], y - top[2], middle[2] - top[2])

        local rEnd = interpolate(colorTop[1], colorBottom[1], y - top[2], bottom[2] - top[2])
        local gEnd = interpolate(colorTop[2], colorBottom[2], y - top[2], bottom[2] - top[2])
        local bEnd = interpolate(colorTop[3], colorBottom[3], y - top[2], bottom[2] - top[2])

        renderer.gradient(xStart, y, xEnd - xStart, 1, rStart, gStart, bStart, 255, rEnd, gEnd, bEnd, 255, true)
    end

    -- Handle the flat top triangle
    for y = middle[2], bottom[2], 1 do
        local alpha = (y - middle[2]) / (bottom[2] - middle[2])

        local xStart = interpolate(middle[1], bottom[1], y - middle[2], bottom[2] - middle[2])
        local xEnd = interpolate(top[1], bottom[1], y - top[2], bottom[2] - top[2])

        local rStart = interpolate(colorMiddle[1], colorBottom[1], y - middle[2], bottom[2] - middle[2])
        local gStart = interpolate(colorMiddle[2], colorBottom[2], y - middle[2], bottom[2] - middle[2])
        local bStart = interpolate(colorMiddle[3], colorBottom[3], y - middle[2], bottom[2] - middle[2])

        local rEnd = interpolate(colorTop[1], colorBottom[1], y - top[2], bottom[2] - top[2])
        local gEnd = interpolate(colorTop[2], colorBottom[2], y - top[2], bottom[2] - top[2])
        local bEnd = interpolate(colorTop[3], colorBottom[3], y - top[2], bottom[2] - top[2])

        renderer.gradient(xStart, y, xEnd - xStart, 1, rStart, gStart, bStart, 255, rEnd, gEnd, bEnd, 255, true)
    end
end

-- normalize to [-180, 180]
local normalize_yaw_180 = function(yaw)
    return (yaw + 180) % 360 - 180
end

-- normalize to [0, 360]
local normalize_yaw_360 = function( yaw )
    return yaw % 360
end

local function lerp(a, b, t)
    return a + (b - a) * t
end

local function outCode(x, y)
    local xmin, ymin, xmax, ymax = 0, 0, screenWidth, screenHeight
    local code = 0
    if x < xmin then
        code = bit.bor(code, 1)
    elseif x > xmax then
        code = bit.bor(code, 2)
    end
    if y < ymin then
        code = bit.bor(code, 4)
    elseif y > ymax then
        code = bit.bor(code, 8)
    end
    return code
end

local function cohenSutherlandClip(x1, y1, x2, y2)
    local xmin, ymin, xmax, ymax = 0, 0, screenWidth, screenHeight
    local code1 = outCode(x1, y1)
    local code2 = outCode(x2, y2)
    local accept = false
    local done = false

    while not done do
        if bit.band(code1, code2) ~= 0 then
            done = true
        elseif bit.bor(code1, code2) == 0 then
            accept = true
            done = true
        else
            local codeOut = code1 ~= 0 and code1 or code2
            local x, y

            if bit.band(codeOut, 8) ~= 0 then
                x = x1 + (x2 - x1) * (ymax - y1) / (y2 - y1)
                y = ymax
            elseif bit.band(codeOut, 4) ~= 0 then
                x = x1 + (x2 - x1) * (ymin - y1) / (y2 - y1)
                y = ymin
            elseif bit.band(codeOut, 2) ~= 0 then
                y = y1 + (y2 - y1) * (xmax - x1) / (x2 - x1)
                x = xmax
            elseif bit.band(codeOut, 1) ~= 0 then
                y = y1 + (y2 - y1) * (xmin - x1) / (x2 - x1)
                x = xmin
            end

            if codeOut == code1 then
                x1, y1 = x, y
                code1 = outCode(x1, y1)
            else
                x2, y2 = x, y
                code2 = outCode(x2, y2)
            end
        end
    end

    return accept, x1, y1, x2, y2
end

local function dotProduct(v1, v2)
    return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
end

local sphere_points_method = ui.new_checkbox('LUA', 'A', 'Alternative Sphere Points')

local function generateSpherePoints_1(center, radius)
    local points = {}

    local dist = center:dist(camera.pos)
    local in_fov = isObjectInFOV(center.x, center.y, center.z, radius)
    local percentOf = math_percentof(dist, 85)
    local value = in_fov and math_clamp(math_floor(30 - math_percent(percentOf, 30)), 4, 30) or 0
    value = value == 0 and value or math_clamp(math_floor((value/100)*(radius*100)), 4, 30)

    local stepTheta = math_pi / value
    local stepPhi = 2 * math_pi / value
    local clampDist = math_clamp(dist/(radius*100), 1, 100)

    for theta = 0, math_pi, stepTheta * clampDist do
        local sinTheta = math_sin(theta)
        local cosTheta = math_cos(theta)

        for phi = 0, 2 * math_pi, stepPhi do
            local x = radius * sinTheta * math_cos(phi)
            local y = radius * sinTheta * math_sin(phi)
            local z = radius * cosTheta

            -- Use the vector library for addition and normalization (if needed)
            table_insert(points, vector(center.x + x, center.y + y, center.z + z))
        end
    end
    return points
end

local function generateSpherePoints_2(center, radius)
    local points = {}

    local dist = center:dist(camera.pos)--distance(center, {camera.pos.x, camera.pos.y, camera.pos.z}) -- Calculate distance from the camera to the sphere's center
    local in_fov = isObjectInFOV(center.x, center.y, center.z, radius)
    local percentOf = math_percentof(dist, 85)
    local value = in_fov and math_clamp(math_floor(30 - math_percent(percentOf, 30)), 4, 30) or 0
    value = (value == 0 and value or math_clamp(math_floor((value/100)*(radius*100)), 4, 30))
    --print('dist: '..dist..' | value: '..value..' | radius: '..radius)
    
    local step = math_pi/value
    for theta = 0, math_pi, step*math_clamp(dist/(radius*100), 1, 100) do
        for phi = 0, 2*math_pi, step do
            local x = center.x + radius * math_sin(theta) * math_cos(phi)
            local y = center.y + radius * math_sin(theta) * math_sin(phi)
            local z = center.z + radius * math_cos(theta)
            --local point_dist = distance({x, y, z}, {camera.pos.x, camera.pos.y, camera.pos.z})
            --if point_dist < dist then 
                table_insert(points, vector(x, y, z))
            --end
        end
    end
    return points
end

local function generateSpherePoints(center, radius)
    if ui.get(sphere_points_method) then
        return generateSpherePoints_2(center, radius)
    else
        return generateSpherePoints_1(center, radius)
    end
end

local function setYawAndPitch()
    -- Get the current cursor position
    local point = ffi.new('POINT[1]')
    if GetCursorPos(point) == 0 then
        return false, 'Failed to get cursor position.'
    end
    
    -- Calculate the difference from the screen center
    local deltaX = point[0].x - screenCenterX
    local deltaY = point[0].y - screenCenterY
    
    -- Translate the difference into yaw and pitch changes
    local yawChange = deltaX * sensitivity
    local pitchChange = deltaY * sensitivity
    camera.yaw = normalize_yaw_360(camera.yaw + yawChange)
    camera.pitch = normalize_yaw_180(camera.pitch + pitchChange)

    -- Reset the cursor to the screen center
    SetCursorPos(screenCenterX, screenCenterY)
    
    return true
end

-- Constants for physics
local GRAVITY = 0.1
local BOUNCE = 0.8
local FRICTION = 0.98

-- Sphere object
local Sphere = {}
Sphere.__index = Sphere

function Sphere:new(center, radius, collision)
    local obj = setmetatable({}, self)
    obj.center = center
    obj.pos = center
    obj.width, obj.height, obj.length = radius, radius, radius
    obj.radius = radius
    obj.points = generateSpherePoints(type(center) == 'cdata' and center or vector(unpack(center)), radius)
    obj.data = {}
    obj.collision = collision or true
    obj.bounce = 0.8
    
    -- Physics attributes
    obj.velocity = vector(0, 0, 0) -- Initialize with no velocity

    -- Physics attributes
    obj.velocity = vector(0, 0, 0) -- Initialize with no velocity
    obj.angularVelocity = vector(0, 0, 0) -- Initialize with no angular velocity

    return obj
end

function Sphere:handleCollisions()
    -- Check for collision with y=0 axis
    if self.center.y - self.radius <= 0 then
        self.velocity.y = -self.velocity.y * self.bounce
        
        -- Convert some linear velocity to angular velocity when bouncing
        self.angularVelocity.x = self.angularVelocity.x + self.velocity.z * 0.1
        self.angularVelocity.z = self.angularVelocity.z - self.velocity.x * 0.1
    end

    -- Simulate ground friction when sphere is touching the ground
    if self.center.y - self.radius <= 0.01 then -- Close enough to the ground
        self.velocity.x = self.velocity.x * FRICTION
        self.velocity.z = self.velocity.z * FRICTION
    end
end

function Sphere:setPosition(x, y, z)
    local pos = (x and not y and not z) and x or vector(x, y, z)
    local d_pos = pos - self.center

    self.center = pos
    self.pos = pos
    
    for i=1, #self.points do
        local point = self.points[i]
        point = point + d_pos
    end
end

function Sphere:isCollidingWithPoint(px, py, pz, camRadius)
    camRadius = camRadius or 0  -- default to point collision if no radius provided

    local distance = self.center:dist(vector(px, py, pz))
    return distance <= (self.radius + camRadius)
end

function Sphere:isCollidingWithObject(x, y, z, w, h, l, camRadius)
    camRadius = camRadius or 0
    local closestX = math_clamp(self.center.x, x, x + w)
    local closestY = math_clamp(self.center.y, y, y + h)
    local closestZ = math_clamp(self.center.z, z, z + l)

    local distanceSquared = (self.center - vector(closestX, closestY, closestZ)):length2d()
    if distanceSquared <= (self.radius + camRadius)^2 then
        local collisionNormal = vector(
            closestX == x and -1 or (closestX == x + w and 1 or 0),
            closestY == y and -1 or (closestY == y + h and 1 or 0),
            closestZ == z and -1 or (closestZ == z + l and 1 or 0)
        )
        return true, {"left", "right", "top", "bottom", "back", "front"}, collisionNormal
    end
    return false, nil, nil
end


--local lightDirection = vector(50, 2, 50)  -- Example light coming from above
--lightDirection:normalized()

function Sphere:update()
    -- Apply gravity to the sphere's vertical velocity
    if self.center.y - self.radius > 0 then
        self.velocity.y = self.velocity.y - GRAVITY 
    end

    -- Update the sphere's position based on its velocity
    local setpos = self.center + self.velocity
    self:setPosition(setpos)

    -- Handle collisions and bounce
    self:handleCollisions()

    self.points = isSphereInView(self.center, self.radius) and generateSpherePoints(self.center, self.radius) or {}
    self.data = #self.points < #self.data and {} or self.data
    -- Compute shading based on light direction
    --local lightDirection = self.center:to(camera.pos)
    --lightDirection:normalized()

    for i=1, #self.points do
        local point = self.points[i]
        local relativePosition = point - self.center

        -- Rotate around Y-axis (based on X angular velocity)
        local rotatedX = relativePosition.x * math_cos(self.angularVelocity.x) - relativePosition.z * math_sin(self.angularVelocity.x)
        local rotatedZ = relativePosition.x * math_sin(self.angularVelocity.x) + relativePosition.z * math_cos(self.angularVelocity.x)

        -- Rotate around X-axis (based on Z angular velocity)
        local rotatedY = relativePosition.y * math_cos(self.angularVelocity.z) + rotatedZ * math_sin(self.angularVelocity.z)
        rotatedZ = -relativePosition.y * math_sin(self.angularVelocity.z) + rotatedZ * math_cos(self.angularVelocity.z)

        -- Update the point
        point.x = self.center.x + rotatedX
        point.y = self.center.y + rotatedY
        point.z = self.center.z + rotatedZ 
        -- Compute the normal of the point on the sphere
        local normal = (point - self.center):normalized()

        -- Compute the direction from the point to the light
        local dirToLight = (lightPosition - point):normalized()

        -- Calculate the dot product between the point's normal and the direction to the light
        local dot = normal:dot(dirToLight)
        
        -- Based on dot product, compute shading intensity (range between 0 and 1)
        local intensity = (dot + 1) / 2

        -- Check if the point is front-facing
        if normal:dot(camera.pos - point) > 0 then
            self.last_projected = self.last_projected or {}
            local x_projected, y_projected = project(point.x, point.y, point.z)
            self.last_projected.x, self.last_projected.y = x_projected or self.last_projected.x, y_projected or self.last_projected.y

            x_projected, y_projected = x_projected or self.last_projected.x, y_projected or self.last_projected.y

            if x_projected and y_projected then
                self.data[i] = {true, x_projected, y_projected, intensity}
            else
                self.data[i] = {false, false, false, 0}
            end
        else
            self.data[i] = {false, false, false, 0}
        end
    end
end
function Sphere:update2()
    if self.center.y - self.radius > 0 then
        self.velocity.y = self.velocity.y - GRAVITY 
    end
    self:setPosition(self.center + self.velocity)

    self:handleCollisions()

    self.points = isSphereInView(self.center, self.radius) and generateSpherePoints(self.center, self.radius) or {}
    self.data = #self.points < #self.data and {} or self.data

    local cosX = math_cos(self.angularVelocity.x)
    local sinX = math_sin(self.angularVelocity.x)
    local cosZ = math_cos(self.angularVelocity.z)
    local sinZ = math_sin(self.angularVelocity.z)
    
    for i, point in ipairs(self.points) do
        local relativePosition = point - self.center

        local rotatedX = relativePosition.x * cosX - relativePosition.z * sinX
        local rotatedZ = relativePosition.x * sinX + relativePosition.z * cosX
        local rotatedY = relativePosition.y * cosZ + rotatedZ * sinZ
        rotatedZ = -relativePosition.y * sinZ + rotatedZ * cosZ

        point.x, point.y, point.z = self.center.x + rotatedX, self.center.y + rotatedY, self.center.z + rotatedZ 
        local normal = (point - self.center):normalized()
        local dirToLight = (lightPosition - point):normalized()
        local dot = normal:dot(dirToLight)
        local intensity = (dot + 1) / 2

        if normal:dot(camera.pos - point) > 0 then
            local x_projected, y_projected = project(point.x, point.y, point.z)
            self.data[i] = x_projected and y_projected and {true, x_projected, y_projected, intensity} or {false, false, false, 0}
        else
            self.data[i] = {false, false, false, 0}
        end
    end
end

function Sphere:render()
    for i=1, #self.data do
        local accept, x_projected, y_projected, intensity = unpack(self.data[i])
        if accept then
            local r, g, b = 255 * intensity, 255 * intensity, 255 * intensity
            renderer.line(x_projected, y_projected, x_projected+1, y_projected, r, g, b, 255)
            --renderer.rectangle(x_projected, y_projected, 4, 4, r, g, b, 255)
        end
    end
end

-- Pyramid object
local Pyramid = {}
Pyramid.__index = Pyramid

function Pyramid:new(x, y, z, w, h, l, baseVertices, apex, collision)
    local obj = setmetatable({}, self)
    obj.pos = vector(x, y, z)
    obj.width, obj.height, obj.length = w, h, l
    obj.vertices = baseVertices
    table_insert(obj.vertices, apex)
    obj.edges = {
        {1, 2}, {2, 3}, {3, 4}, {4, 1},
        {1, 5}, {2, 5}, {3, 5}, {4, 5}
    }
    obj.data = {}
    obj.collision = collision or true
    return obj
end

function Pyramid:setPosition(x, y, z)
    self.pos = vector(x, y, z)

    local dx = x - self.vertices[1][1]
    local dy = y - self.vertices[1][2]
    local dz = z - self.vertices[1][3]

    for i=1, #self.vertices do
        local vertex = self.vertices[i]
        vertex[1] = vertex[1] + dx
        vertex[2] = vertex[2] + dy
        vertex[3] = vertex[3] + dz
    end
end

function Pyramid:isCollidingWithPoint(px, py, pz)
    if not self.collision then
		return false
	end

    local minX, maxX, minY, maxY, minZ, maxZ = math_huge, -math_huge, math_huge, -math_huge, math_huge, -math_huge
    for i=1, #self.vertices do
        local vertex = self.vertices[i]
        minX = math_min(minX, vertex[1])
        maxX = math_max(maxX, vertex[1])
        minY = math_min(minY, vertex[2])
        maxY = math_max(maxY, vertex[2])
        minZ = math_min(minZ, vertex[3])
        maxZ = math_max(maxZ, vertex[3])
    end
    return px >= minX and px <= maxX and py >= minY and py <= maxY and pz >= minZ and pz <= maxZ
end

function Pyramid:update()
    for i=1, #self.edges do
        local edge = self.edges[i]
        local v1 = self.vertices[edge[1]]
        local v2 = self.vertices[edge[2]]
        v2 = v2 or {0, 0, 0}
        local x1_projected, y1_projected = project(v1[1], v1[2], v1[3])
        local x2_projected, y2_projected = project(v2[1], v2[2], v2[3])
        if x1_projected and y1_projected and x2_projected and y2_projected then
            local accept, x1_clipped, y1_clipped, x2_clipped, y2_clipped = cohenSutherlandClip(x1_projected, y1_projected, x2_projected, y2_projected)
            self.data[i] = {accept, x1_clipped, y1_clipped, x2_clipped, y2_clipped}
        else
            self.data[i] = {false, false, false, false, false}
        end
    end
end

function Pyramid:render()
    for i=1, #self.data do
        local accept, x1_clipped, y1_clipped, x2_clipped, y2_clipped = unpack(self.data[i])
        if accept then
            renderer.line(x1_clipped, y1_clipped, x2_clipped, y2_clipped, 255, 0, 0, 255)
        end
    end
end

-- Cube object
local Cube = {}
Cube.__index = Cube

-- Define the normals for the six faces of the cube
local defaultFaceNormals = {
    vector(0, 0, -1),  -- Front face
    vector(0, 0, 1),   -- Back face
    vector(-1, 0, 0),  -- Left face
    vector(1, 0, 0),   -- Right face
    vector(0, -1, 0),  -- Bottom face
    vector(0, 1, 0)    -- Top face
}

function Cube:new(x, y, z, w, h, l, vertices, edges, collision)
    local obj = setmetatable({}, self)
    obj.pos = vector(x, y, z)
    obj.width, obj.height, obj.length = w, h, l
    obj.vertices = vertices
    obj.edges = edges
    obj.data = {}
    obj.collision = collision or true

    -- Precompute face polygons
    obj.faces = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {1, 2, 6, 5},
        {2, 3, 7, 6},
        {3, 4, 8, 7},
        {4, 1, 5, 8}
    }
    
    -- Precompute face normals
    obj.faceNormals = obj:computeFaceNormals() or defaultFaceNormals

    return obj
end

function Cube:setPosition(x, y, z)
    self.pos = vector(x, y, z)
    local d_vec = (x and not y) and x or vector(x, y, z)
    d_vec = d_vec - self.vertices[1]
    
    local new_vertex = {}
    for i=1, #self.vertices do
        local vertex = self.vertices[i]
        new_vertex[i] = vertex + d_vec
    end

    self.vertices = new_vertex
end

function Cube:isCollidingWithPoint(px, py, pz)
    if not self.collision then
		return false
	end

    local minX, maxX, minY, maxY, minZ, maxZ = math.huge, -math.huge, math.huge, -math.huge, math.huge, -math.huge
    for i=1, #self.vertices do
        local vertex = self.vertices[i]
        minX = math.min(minX, vertex.x)
        maxX = math.max(maxX, vertex.x)
        minY = math.min(minY, vertex.y)
        maxY = math.max(maxY, vertex.y)
        minZ = math.min(minZ, vertex.z)
        maxZ = math.max(maxZ, vertex.z)
    end
    return px >= minX and px <= maxX and py >= minY and py <= maxY and pz >= minZ and pz <= maxZ
end

function Cube:isCollidingWithObject(x, y, z, w, h, l)
    local obj = {x=x, y=y, z=z, width=w, height=h, length=l}
    
    if not self.collision then
		return false, nil
	end

    local minX, maxX, minY, maxY, minZ, maxZ = math.huge, -math.huge, math.huge, -math.huge, math.huge, -math.huge
    for i=1, #self.vertices do
        local vertex = self.vertices[i]
        minX = math.min(minX, vertex.x)
        maxX = math.max(maxX, vertex.x)
        minY = math.min(minY, vertex.y)
        maxY = math.max(maxY, vertex.y)
        minZ = math.min(minZ, vertex.z)
        maxZ = math.max(maxZ, vertex.z)
    end
    
    -- Object's bounding box
    local objMinX, objMaxX = obj.x, obj.x + obj.width
    local objMinY, objMaxY = obj.y, obj.y + obj.height
    local objMinZ, objMaxZ = obj.z, obj.z + obj.length

    -- Check for overlap in x, y and z axes
    if (minX < objMaxX and maxX > objMinX) and 
       (minY < objMaxY and maxY > objMinY) and 
       (minZ < objMaxZ and maxZ > objMinZ) then
       
       -- Determine the collision direction
       local collisionDirections = {}

       if maxX > objMinX and minX < objMinX then table_insert(collisionDirections, "right") end
       if minX < objMaxX and maxX > objMaxX then table_insert(collisionDirections, "left") end
       
       if maxY > objMinY and minY < objMinY then table_insert(collisionDirections, "top") end
       if minY < objMaxY and maxY > objMaxY then table_insert(collisionDirections, "bottom") end
       
       if maxZ > objMinZ and minZ < objMinZ then table_insert(collisionDirections, "front") end
       if minZ < objMaxZ and maxZ > objMaxZ then table_insert(collisionDirections, "back") end

       return true, collisionDirections
    end
    
    return false, nil
end


function Cube:computeFaceNormals()
    local faceNormals = {}

    -- The order of vertices here assumes counter-clockwise winding
    for i=1, #self.faces do
        local face = self.faces[i]
        local v1 = self.vertices[face[1]]
        local v2 = self.vertices[face[2]]
        local v3 = self.vertices[face[3]]

        local a = vector(v2.x - v1.x, v2.y - v1.y, v2.z - v1.z)
        local b = vector(v3.x - v1.x, v3.y - v1.y, v3.z - v1.z)

        -- Compute the cross product to get the face normal
        local normal = crossProduct(a, b):normalized()
        table_insert(faceNormals, normal)
    end

    return faceNormals
end

function Cube:update()
    for i=1, #self.edges do
        local edge = self.edges[i]

        local v1 = self.vertices[edge[1]]
        local v2 = self.vertices[edge[2]]

        self.last_projected = self.last_projected or {}
        local x1_projected, y1_projected = project(v1.x, v1.y, v1.z)
        local x2_projected, y2_projected = project(v2.x, v2.y, v2.z)

        if x1_projected and y1_projected and x2_projected and y2_projected then
            local accept, x1_clipped, y1_clipped, x2_clipped, y2_clipped = cohenSutherlandClip(x1_projected, y1_projected, x2_projected, y2_projected)
            if (x1_projected and y1_projected and x1_projected >= 0 and y1_projected <= screenWidth and y1_projected >= 0 and y1_projected <= screenHeight) or
                (x2_projected and y2_projected and x2_projected >= 0 and y2_projected <= screenWidth and y2_projected >= 0 and y2_projected <= screenHeight) then
                accept = true
            else
                accept = false
            end
            print(tostring(accept)..', '..table.concat({x1_clipped, y1_clipped, x2_clipped, y2_clipped}, ', '))
            self.data[i] = {accept, x1_clipped, y1_clipped, x2_clipped, y2_clipped}
        else
            self.data[i] = {false, false, false, false, false}
        end
    end
end
  
function Cube:render()
    -- Define the six faces of the cube
    local faces = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {1, 2, 6, 5},
        {2, 3, 7, 6},
        {3, 4, 8, 7},
        {4, 1, 5, 8}
    }

    -- Calculate centers of each face and associate it with its index
    local faceCenters = {}
    for i, face in ipairs(faces) do
        local x, y, z = 0, 0, 0
        for _, vertexIndex in ipairs(face) do
            x = x + self.vertices[vertexIndex].x
            y = y + self.vertices[vertexIndex].y
            z = z + self.vertices[vertexIndex].z
        end
        local center = vector(x / 4, y / 4, z / 4)
        table_insert(faceCenters, {index = i, center = center, distance = center:dist(camera.pos)})
    end

    -- Sort faces by distance to camera
    table.sort(faceCenters, function(a, b)
        return a.distance > b.distance
    end)

    -- Render faces based on sorted order
    for i=1, #faceCenters do
        local faceData = faceCenters[i]
        local face = faces[faceData.index]
        local polygonVertices = {}
        local colors = {}

        for i2=1, #face do
            local vertexIndex = face[i2]
            local vertex = self.vertices[vertexIndex]
            local data = self.data[vertexIndex]

            if data and data[1] then  -- Check if the vertex is within the screen boundaries
                table_insert(polygonVertices, {data[2], data[3]})

                -- Calculate direction from vertex to light
                local dirToLight = (lightPosition - vertex):normalized()

                -- Calculate dot product between face normal and light direction
                local dot = dotProduct(self:computeFaceNormals()[faceData.index], dirToLight)

                -- Based on dot product, compute shading intensity (range between 0.2 and 1 for visualization)
                local intensity = 0.2 + 0.8 * (dot + 1) / 2

                -- Use this intensity to determine the vertex color
                local r = 0 * intensity
                local g = 0 * intensity
                local b = 255 * intensity

                table_insert(colors, {r, g, b})
            end
        end

        if #polygonVertices == 4 then  -- Ensure we have 4 vertices to form a polygon
            -- Get the colors for the four corners
            local c1 = colors[1]
            local c2 = colors[2]
            local c3 = colors[3]
            local c4 = colors[4]

            -- Render the two triangles that make up the face using gradient shading
            renderer.gradientTriangle(polygonVertices[1], polygonVertices[2], polygonVertices[3], c1, c2, c3)
            renderer.gradientTriangle(polygonVertices[1], polygonVertices[3], polygonVertices[4], c1, c3, c4)
        end
    end
end


local objects = {
    cubes = {},
    pyramids = {},
    spheres = {}
}

-- Factory for creating 3D objects
local ObjectFuckery = {}

function ObjectFuckery:createCube(x, y, z, w, h, l, collision)
    local vertices = {
        vector(x, y, z),
        vector(x + w, y, z),
        vector(x + w, y + h, z),
        vector(x, y + h, z),
        vector(x, y, z + l),
        vector(x + w, y, z + l),
        vector(x + w, y + h, z + l),
        vector(x, y + h, z + l)
    }
    
    local edges = {
        {1, 2},
        {2, 3},
        {3, 4},
        {4, 1},
        {5, 6},
        {6, 7},
        {7, 8},
        {8, 5},
        {1, 5},
        {2, 6},
        {3, 7},
        {4, 8}
    }
    table_insert(objects['cubes'], Cube:new(x, y, z, w, h, l, vertices, edges, collision))
    return objects['cubes'][#objects['cubes']] 
end

function ObjectFuckery:createSphere(x, y, z, radius, collision)
    table_insert(objects['spheres'], Sphere:new(vector(x, y, z), radius, collision))
    return objects['spheres'][#objects['spheres']]
end

-- For the pyramid, assuming the apex is directly above the center of the base:
function ObjectFuckery:createPyramid(x, y, z, baseWidth, baseLength, height, collision)
    local baseVertices = {
        {x - baseWidth / 2, y, z - baseLength / 2},
        {x + baseWidth / 2, y, z - baseLength / 2},
        {x + baseWidth / 2, y, z + baseLength / 2},
        {x - baseWidth / 2, y, z + baseLength / 2}
    }
    local apex = {x, y + height, z}
    table_insert(objects['pyramids'], Pyramid:new(x, y, z, baseWidth, baseLength, height, baseVertices, apex, collision))
    return objects['pyramids'][#objects['pyramids']]
end

local factory = ObjectFuckery
--[[
        y
        |
        |
        |
        |
        |
        |
        +-----------x
       /
      /
     /
    z
]]
factory:createCube(math.random(-50, 50), 0, math.random(-50, 50), math.random(1, 20), math.random(1, 20), math.random(1, 20) )
factory:createCube(math.random(-50, 50), 0, math.random(-50, 50), math.random(1, 20), math.random(1, 20), math.random(1, 20) )
factory:createCube(math.random(-50, 50), 0, math.random(-50, 50), math.random(1, 20), math.random(1, 20), math.random(1, 20) )
factory:createCube(math.random(-50, 50), 0, math.random(-50, 50), math.random(1, 20), math.random(1, 20), math.random(1, 20) )
for i=1, 15 do
    factory:createSphere(math.random(-50, 50), math.random(20, 100), math.random(-50, 50), math.random(300, 800)/100)
end

--factory:createCube(-50, -50, -50, 500, 1, 500 )
--factory:createPyramid(5, 1, 5, 2, 2, 3)

--local player_esp = factory:createCube(0, 0, 0, 1, 2, 1, false)
--player_esp.collision = false

-- States for mouse lock
local STATES = {
    LOCKED = "LOCKED",
    UNLOCKED = "UNLOCKED",
    PRESSED = "PRESSED"
}

-- Initialize the state to UNLOCKED
local state = STATES.UNLOCKED
local was_locked, last_msg

-- State handlers for each possible state of the mouse lock
local handlers = {
    [STATES.LOCKED] = function()
        setYawAndPitch()

        if (not isInWindow() or ui.is_menu_open()) then
            was_locked = true
            showCursor()
            return STATES.UNLOCKED, 'menu open, unlocking'
        elseif client.key_state(key_codes.x) then
            return STATES.PRESSED, 'key pressed'
        end
        return STATES.LOCKED, 'locked'
    end,

    [STATES.UNLOCKED] = function()
        if client.key_state(key_codes.x) then
            return STATES.PRESSED, 'key pressed, locking'
        elseif (isInWindow() and not ui.is_menu_open()) and was_locked then
            was_locked = false
            hideCursor()
            return STATES.LOCKED, 'menu closed, locking'
        end
        return STATES.UNLOCKED, 'unlocked'
    end,

    [STATES.PRESSED] = function()
        if not client.key_state(key_codes.x) then
            if was_locked then
                was_locked = false
                showCursor()
                return STATES.UNLOCKED, 'pressed, unlocking'
            else
                was_locked = true
                hideCursor()
                return STATES.LOCKED, 'pressed, locking'
            end
        end
        return STATES.PRESSED, 'key down'
    end
}

-- Function to handle mouse lock based on its current state
local function handleMouseLock()
    local newState, message = handlers[state]()
    state = newState
    if message and message ~= last_msg then
        last_msg = message
        print(message)
    end
end

-- Function to handle the input for the camera movement and mouse lock
local function handleInput()
    -- Normalize camera yaw for 360 degrees
    camera.yaw = normalize_yaw_360(camera.yaw)

    -- Calculate trigonometric values for movement calculations
    local cos_yaw = math.cos(math.rad(camera.yaw))
    local sin_yaw = math.sin(math.rad(camera.yaw))
    local cos_pitch = math.cos(math.rad(camera.pitch))
    local sin_pitch = freecam and math.sin(math.rad(camera.pitch)) or 0

    -- Determine movement speed based on whether the camera is grounded or not
    local movementSpeed = camera.isGrounded and camera.speed or camera.speed * camera.airControl

    local oldPos = vector(camera.pos.x, camera.pos.y, camera.pos.z)  -- store the old position of the camera

    -- Update camera velocity based on user inputs
    -- W, A, S, D for movement, Q and E for yaw, R and F for pitch
    -- Space for jumping (if grounded)
    local movementDirections = {
        {key_codes.s, vector(sin_yaw * cos_pitch, sin_pitch, cos_yaw * cos_pitch)},
        {key_codes.w, -vector(sin_yaw * cos_pitch, sin_pitch, cos_yaw * cos_pitch)},
        {key_codes.a, vector(cos_yaw, 0, -sin_yaw)},
        {key_codes.d, -vector(cos_yaw, 0, -sin_yaw)}
    }

    for _, dir in ipairs(movementDirections) do
        if client.key_state(dir[1]) then
            camera.velocity = camera.velocity + dir[2] * movementSpeed
        end
    end

    camera.yaw = client.key_state(key_codes.q) and normalize_yaw_360(camera.yaw - camera.speed*2) or camera.yaw
    camera.yaw = client.key_state(key_codes.e) and normalize_yaw_360(camera.yaw + camera.speed*2) or camera.yaw
    camera.pitch = client.key_state(key_codes.r) and normalize_yaw_180(camera.pitch + camera.speed) or camera.pitch
    camera.pitch = client.key_state(key_codes.f) and normalize_yaw_180(camera.pitch - camera.speed) or camera.pitch

    if client.key_state(key_codes.space) and camera.isGrounded then
        camera.velocity.y = camera.jumpForce
        camera.isGrounded = false
    end

    handleMouseLock()
    return oldPos
end

local function updateLightDirection()
    -- Compute the new light position based on the angle
    lightPosition.x = math.cos(lightAngle) * lightCircleRadius
    lightPosition.z = math.sin(lightAngle) * lightCircleRadius

    -- Compute the direction from the light position to the origin (0,0,0)
    lightDirection = vector(-lightPosition.x, -lightPosition.y, -lightPosition.z)  -- Assuming Y is downwards
    lightDirection = lightDirection:normalized()  -- Normalize to ensure it's a unit vector

    -- Update the lightAngle for the next frame
    lightAngle = lightAngle + 0.01  -- Adjust this value to change the speed of the light's movement
    if lightAngle > 2 * math.pi then
        lightAngle = lightAngle - 2 * math.pi
    end
end

local function renderLight()
    local lightScreenX, lightScreenY = project(lightPosition.x, lightPosition.y, lightPosition.z)
    for i = 0, lightRadius do
        local alpha = 255 * (1 - i/lightRadius)
        if lightScreenX and lightScreenY then
            renderer.gradient(lightScreenX - i, lightScreenY, 2*i, 2*i, 255, 255, 255, alpha, 255, 255, 255, 0, true)
        end
    end
end

-- Function to draw the player position indicator
local function drawPlayerPosition()
    local x_projected, y_projected = project(camera.pos.x, camera.pos.y, camera.pos.z)
    local size = 5  -- size of the crosshair

    -- Draw vertical line
    renderer.line(x_projected, y_projected - size, x_projected, y_projected + size, 0, 255, 0, 255)
    -- Draw horizontal line
    renderer.line(x_projected - size, y_projected, x_projected + size, y_projected, 0, 255, 0, 255)
end

local function calculateCenter(vertices)
    local sumX, sumY, sumZ = 0, 0, 0
    local count = #vertices

    for i = 1, count do
        sumX = sumX + vertices[i].x
        sumY = sumY + vertices[i].y
        sumZ = sumZ + vertices[i].z
    end

    return vector(sumX / count, sumY / count, sumZ / count)
end

local function sortObjectsByDistance(shape)
    table.sort(objects[shape], function(a, b)
        local aCenter, bCenter
        
        if shape == 'spheres' then
            aCenter = a.center
            bCenter = b.center
        else
            aCenter = calculateCenter(a.vertices)
            bCenter = calculateCenter(b.vertices)
        end
        
        local aDist = aCenter:dist(camera.pos)
        local bDist = bCenter:dist(camera.pos)
        return aDist > bDist
    end)
end

local function calculateCenterForSphere(sphere)
    return sphere.center
end

local function getCenter(object, shape)
    if shape == 'spheres' then
        return object.center
    else
        return calculateCenter(object.vertices)
    end
end

local shapes = {'pyramids', 'spheres', 'cubes'}
local allObjects = {}

local function handleCollision(obj1, obj2, collisionDirections, collisionNormal)
    -- Advanced collision response considering mass, velocity and coefficient of restitution (COR) for elastic collision
    local mass1 = obj1.mass or 1
    local mass2 = obj2.mass or 1
    local velocity1 = obj1.velocity or vector(0, 0, 0)
    local velocity2 = obj2.velocity or vector(0, 0, 0)
    local COR = 0.1 -- Coefficient of Restitution, you can adjust this value

    -- Handling Sphere-Sphere and Sphere-Cube collisions
    local overlapCorrection
    if obj1.radius and obj2.radius then
        collisionNormal = (obj2.center - obj1.center):normalized()
        local overlap = obj1.radius + obj2.radius - (obj2.center - obj1.center):length()
        overlapCorrection = collisionNormal * overlap * 0.5
    elseif (obj1.radius and not obj2.radius) or (not obj1.radius and obj2.radius) then
        local sphere, cube
        if obj1.radius then
            sphere = obj1
            cube = obj2
        else
            sphere = obj2
            cube = obj1
        end
        collisionNormal = (sphere.center - calculateCenter(cube.vertices)):normalized()
        local overlap = sphere.radius - (sphere.center - calculateCenter(cube.vertices)):length()
        overlapCorrection = collisionNormal * overlap * 0.5
    end

    local relativeVelocity = velocity2 - velocity1
    local relativeVelocityAlongNormal = relativeVelocity:dot(collisionNormal)
    
    if relativeVelocityAlongNormal > 0 then
        return
    end

    local impulseMagnitude = -(2 * relativeVelocityAlongNormal) / (1/mass1 + 1/mass2) * COR
    local impulse = collisionNormal * impulseMagnitude

    obj1.velocity = velocity1 + (impulse / mass1)
    obj2.velocity = velocity2 - (impulse / mass2)

    -- Adjust positions to prevent objects from sticking together
    if overlapCorrection then
        obj1.pos = obj1.pos - overlapCorrection
        obj2.pos = obj2.pos + overlapCorrection
    else
        local overlapCorrectionFactor = 0.8
        overlapCorrection = collisionNormal * overlapCorrectionFactor
        obj1.pos = obj1.pos - vector(overlapCorrection, overlapCorrection, overlapCorrection)
        obj2.pos = obj2.pos + vector(overlapCorrection, overlapCorrection, overlapCorrection)
    end

    print("Collision detected between " .. (obj1.radius and 'Sphere' or 'Cube') .. " and " .. (obj2.radius and 'Sphere' or 'Cube') .. " with collision directions: " .. (collisionDirections and table.concat(collisionDirections, ', ') or 'None'))
end

local function updatePhysics(oldPos)
    -- Apply physics
    camera.velocity.y = camera.velocity.y + (not freecam and camera.gravity or 0)
    camera.velocity = camera.velocity * (camera.isGrounded and camera.friction or camera.inAirFriction)
    camera.velocity.y = math.max(camera.velocity.y, camera.terminalVelocity)

    -- Update camera position based on velocity
    camera.pos = camera.pos + camera.velocity

    -- Check for ground collision (assuming ground is at y=0)
    if camera.pos.y <= (0 + camera.height) then
        camera.pos.y = 0 + camera.height
        camera.velocity.y = 0
        camera.isGrounded = true
    end

    -- Check for collisions with spheres, pyramids, and cubes
    local shapes_t = {
        {objs = objects['spheres'], name = 'Sphere'},
        {objs = objects['pyramids'], name = 'Pyramid'},
        {objs = objects['cubes'], name = 'Cube'}
    }

    for i=1, #shapes_t do
        local shape = shapes_t[i]
        for i2=1, #shape.objs do
            local obj = shape.objs[i2]      
            for i4=1, #shapes_t do
                local shape2 = shapes_t[i4]
                for i3 = 1, #shape2.objs do
                    if (i2 ~= i3 and i ~= i4)then
                        local otherObject = shape2.objs[i3]
                        local collision_func = obj.isCollidingWithObject or obj.isCollidingWithPoint
                        local isColliding, collisionDirections, collisionNormal = collision_func(obj, otherObject.pos.x, otherObject.pos.y, otherObject.pos.z, otherObject.width, otherObject.height, otherObject.length, otherObject.radius, otherObject.velocity)
                        if isColliding then
                            print(shape.name .. ' Collision with ' .. shape2.name .. 'at Position: ', 'x: ' .. otherObject.pos.x .. ', y: ' .. otherObject.pos.y .. ', z:' .. otherObject.pos.z .. (collisionDirections and (' ,' .. table.concat(collisionDirections, ', ')) or ''))
                            handleCollision(obj, otherObject, collisionDirections, collisionNormal)
                        end
                    end
                end
            end
            
            local collision_func = obj.isCollidingWithObject or obj.isCollidingWithPoint
            local isColliding, collisionDirections, collisionNormal = collision_func(obj, camera.pos.x, camera.pos.y-camera.height, camera.pos.z, camera.width, camera.height, camera.length, camera.radius, camera.velocity)

            if isColliding and collisionNormal then
                -- Transfer some of the object's velocity to the sphere in the direction of the collision
                --local transferFactor = 0.5 -- You can adjust this value
                --obj.velocity = obj.velocity + camera.velocity * camera.velocity:length() * transferFactor
                --print(shape.name .. ' Collision at Camera Position: ', 'x: ' .. camera.pos.x .. ', y: ' .. camera.pos.y .. ', z:' .. camera.pos.z .. (collisionDirections and (' ,' .. table.concat(collisionDirections, ', ')) or ''))
            else
                local is_top = false
                if collisionDirections then
                    is_top = table.concat(collisionDirections, ', '):find('top') and 'top' or false
                end
                if isColliding then
                    if is_top then
                        camera.velocity.y = camera.velocity.y < -0.1 and camera.velocity.y - camera.velocity.y or camera.velocity.y - camera.gravity
                        camera.isGrounded = true
                        camera.pos = oldPos + camera.velocity
                    else
                        camera.pos = oldPos
                    end
                    print(shape.name .. ' Collision at Camera Position: ', 'x: ' .. camera.pos.x .. ', y: ' .. camera.pos.y .. ', z:' .. camera.pos.z .. (collisionDirections and (' ,' .. table.concat(collisionDirections, ', ')) or ''))
                end
            end
        end
    end
end

local function handleObjects()
    local count = 0
    for i=1, #shapes do
        local shape = shapes[i]
        for i2=1, #objects[shape] do
            count = count + 1
            local object = objects[shape][i2]
            object:update()
            allObjects[count] = {object = object, shape = shape, center = getCenter(object, shape)}
        end
    end

    table.sort(allObjects, function(a, b)
        local aDist = a.center:dist(camera.pos)
        local bDist = b.center:dist(camera.pos)
        return aDist > bDist
    end)
    
    --for i = 1, #allObjects do
    --    local object = allObjects[i].object
        --object:update()
   -- end
end

local function update(dt)
    local oldPos = handleInput()
    updatePhysics(oldPos)
    handleObjects()
    updateLightDirection()
    
    dt = globals.frametime()
    client.delay_call(dt, update)
end
update(globals.frametime())
--client.set_event_callback('pre_render', update)

local background = ui.new_checkbox('LUA', 'A', 'Background')
local function draw()
    if ui.get(background) then
        renderer.rectangle(0, 0, screenWidth, screenHeight, 0, 0, 0, 255)
    end
    renderLight()
    for i = 1, #allObjects do
        local object = allObjects[i].object
        object:render()
    end
end
client.set_event_callback('paint_ui', draw)

client.set_event_callback('shutdown', showCursor)
defer(showCursor)
