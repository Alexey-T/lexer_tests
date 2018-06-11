function Animal:eat()
  print "An animal is eating..."
end

function Animal:speak()
  print "An animal is speaking..."
end

function Dog:eat()
  print "A dog is eating..."
end

function Dog:speak()
  print "Wah, wah!"
end

function Cat:speak()
  print "Meoow!"
end

function Human:speak()
  print "Hello!"
end 

function Animal:init(name, age)
  self.name = name
  self.age = age
end

function Dog:init(name, age, master)
  self.super:init(name, age)   -- notice call to superclass's constructor
  self.master = master
end

function Cat:init(name, age)
  self.super:init(name, age)
end

function Human:init(name, age, city)
  self.super:init(name, age)
  self.city = city
end

function Animal:__tostring()
  return "An animal called " .. self.name .. " and aged " .. self.age 
end

function Human:__tostring()
  return "A human called " .. self.name .. " and aged " .. self.age .. ",
         living at " .. self.city
end
