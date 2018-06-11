require("INC_Class.lua")

--===========================

cAnimal=setclass("Animal")

function cAnimal.methods:init(action, cutename) 
	self.superaction = action
	self.supercutename = cutename
end

--==========================

cTiger=setclass("Tiger", cAnimal)

function cTiger.methods:init(cutename) 
	self:init_super("HUNT (Tiger)", "Zoo Animal (Tiger)")
	self.action = "ROAR FOR ME!!"
	self.cutename = cutename
end

--==========================

Tiger1 = cAnimal:new("HUNT", "Zoo Animal")
Tiger2 = cTiger:new("Mr Grumpy")
Tiger3 = cTiger:new("Mr Hungry")
