import Data.List
import System.IO

--Data Types:
maxInt = maxBound::Int
minInt = minBound::Int

bigFlOAT = 3.9333 + 0.4444

always5 :: Int
always5 = 5

sumOfNums = sum [1..1000]
addEx = 5 + 4
modEx = mod 5 4
--or
modEx2 = 5 `mod` 4

neg = 5 + (-4)

num9 = 9 :: Int
sqrtOf9 = sqrt(fromIntegral num9)

--exp log truncate round ceiling floor **(squre) Pi

trueOrfalse = True || False
trueAndfalse = True && False

primeNumbers = [3,5,7,11]

morePrime = primeNumbers ++ [13]

nums = 2:3:4:[]

lenPrime = length morePrime
reversed = reverse morePrime

firstValue = head morePrime
lastValue = last morePrime

first3value = take 3 morePrime

removeprime = drop 3 morePrime

is3in = 3 `elem` morePrime

maxiprime = max morePrime

newList = [2,3,4]

morePrime2 = 2 : morePrime

zeroToTen= [0..10]

evens = [2,4..20]

chars = ['a'..'z']

charsjumped = []

--infinites will not be really created while it is called
infinite = [10,20..]

my2s = take 10 (repeat 2)

many3s = replicate 10 3
cycleList = take 10 [1,2,3]


-- [list ops | list assign, condition of output]
list2 = [x*2 | x <- many3s, x*2 == 6] 

dividelist = [x | x <- [1..500], x `mod` 9 == 0]

soortedList = sort [3,2,1]

sums = zipWith (+) [1,2,3,4][5,6,7,8]

listallbiggerthat5 = filter (>5) morePrime2

upto20 = takeWhile (>20) [2,4..]

-- there are also foldr
multilist = foldl (*) 1 [2,3,4,5]

pow3List = [3^n | n <- [1..], 3^n < 3^11]

multiTable = [[x * y | y <- [1..10]] | x <- [1..10]]

--Tuples
randTuple = (1,"dfhiudshfousdhf")
bobSmith = ("BoB Smith", 52)

bobsName = fst bobSmith
bobsAge = snd bobSmith
names = ["me","you"]
ages = [12,22]

mixedTuple = zip names ages

addMe :: Int->Int->Int

addMe x y = x + y
--this might work with floats bcuz type of parameter is not defined
sumMe x y = x + y

addTuples :: (Int,Int) -> (Int,Int) -> (Int,Int)
addTuples (x,y) (a,b) = (x+a,y+b)

whatAge :: Int -> String

whatAge 16 = "You can drive"
whatAge 18 = "oh yeah"

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial(n - 1) * n

-- 3*factorial(2) = 3 * 2 * 1 * 1

isOdd :: Int -> Bool

isOdd n 
	| n `mod` 2 == 0 = False -- this is even
	| otherwise = True

isEven n = n `mod` 2 == 0

whatGrade :: Int -> String

whatGrade age
	| (age >= 5) && (age <= 7) = "Kindergarden"
	| (age > 7) && (age <= 8) = "elementary school"
	| otherwise = "no care"
batAvgRating :: Double -> Double -> String

batAvgRating hits atBats
	| avg <= 0.200 = "Piece of shit"
	| avg <= 0.250 = "normal"
	| avg <= 0.280 = "so jb"
	| otherwise = "wocao"
	where avg = hits /atBats

getListItems :: [Int] -> String

getListItems [] = "Your list is clean like my ass"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list has " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The first item is " ++ show x ++ " and rest is" ++ show xs

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort(filter (<x) xs) ++ [x] ++ qsort(filter (>=x) xs)
getFirstItems :: String -> String

getFirstItems [] = "You have nothing!"
getFirstItems all@(x:xs) = "The first letter in" ++ all ++ "is" ++ show x

times4 :: Int -> Int

times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

-- The beauty of recursions
multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEqual :: [Char] -> [Char] -> Bool

areStringsEqual [] [] = True
areStringsEqual (x:xs)(y:ys) = x == y && areStringsEqual xs ys
areStringsEqual _ _  = False

doMults :: (Int -> Int) -> Int

doMults func = func 3

num3Times4 = doMults times4

getAddFunction :: Int -> (Int -> Int)

getAddFunction x y = x + y
add3 = getAddFunction 3
fourPlus3 = add3 4

threePlusList = map add3 [3,4,5,6,7]

--Lambda

dbl1To10 = map (\x -> x * 2) [1..10]

-- < > <= >= == /=(not equal to)

doubleEvens y =
	if(y `mod` 2 /= 0)
		then y
		else y * 2

getClass :: Int -> String

getClass n = case n of
	5 -> "Kindergarden"
	6 -> "elementary school"
	_ -> "no"

-- import from other files
-- module SampFunctions (getClass, doubleEvens) where
-- import SampFunctions

data BaseballPlayer = Pitcher
					| Catcher
					| Infielder
					| OutField
					deriving Show

barryBounds :: BaseballPlayer -> Bool
barryBounds OutField = True
barryInOF = print(barryBounds OutField)

data Customer = Customer String String Double
	deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "Yeah Street" 20.50

getBalance :: Customer -> Double 
getBalance (Customer _ _ b) = b

data RPS = Rock | Paper | Scissors
shoot :: RPS -> RPS -> String

shoot Paper Rock = "Paper beats the rock!"

data Shape = Circle Float Float Float| Rectangle Float Float Float Float
	deriving Show

area :: Shape -> Float
area (Rectangle x y x2 y2) = (abs $ x2 - x)*(abs $ y2 - y)

sumValue = putStrLn (show (1+2))

-- or sumValue = putStrLn . show $ 1+2
areaOfCircle = area (Circle 50 60 20)
areaOfRectangle = area $ Rectangle 10 10 100 100

--Type Class
data Employee = Employee {name :: String, position :: String, id :: Int}
	deriving (Eq, Show)

data ShirtSize = S | M | L

instance Eq ShirtSize where
	S == S = True
	M == M = True
	_ == _ = False
	L == L = True

instance Show ShirtSize where
	show S = "Small"
	show M = "Medium"
	show L = "Large"

class Myequal a where
	areEqual :: a -> a -> Bool
instance Myequal ShirtSize where
	areEqual S S = True 
	areEqual M M = True 
	areEqual L L = True 
	areEqual _ _ = False

newSize = areEqual M M


sayHello = do
	putStrLn "What's your name?"
	name <- getLine
	putStrLn $ "Hello" ++ name

writeToFile = do
	theFile <- openFile "test.txt" WriteMode
	hPutStrLn theFile ("asfasfsafas")
	hClose theFile

readFromFile = do
	theFile2 <- openFile "test.txt" ReadMode
	contents <- hGetContents theFile2 
	putStr contents
	hClose theFile2


fib = 1 : 1 :[a + b | (a,b) <- zip fib (tail fib)]
-- [1,1, (created inside) 2]
-- 1st fib = 1, (tail fib) = 1
--[1,1,2] : a:1 + b:1 =2
-- 2st fib = 1 and (tail fib) = 2
-- [1,1,2,3] a : 1 + b : 2 = 3

fib300 = fib !! 300 
