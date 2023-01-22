data Shape = Circle Float Float Float| Rectangle Float Float Float Float
	deriving Show
-- circle needs one parameter to be fulfilled as a Shape variable
-- rectangle needs 2, returns one then another

area :: Shape -> Float
area (Rectangle x y x1 y1) = (x1-x) * (y1-y)

area (Circle x y z) = (x * y * z)