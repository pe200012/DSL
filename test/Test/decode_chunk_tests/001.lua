-- test.lua
local a = 1
local b = nil
local c = "the answer is 42"
local d = 3.14

local function foo(x)
    return x + 1
end

local function clo(p)
    return p + a
end

print(clo(foo(1)))
