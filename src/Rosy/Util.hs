module Rosy.Util where

type DPoint = (Double,Double)
type DVector = DPoint

-- | Converts an angle in radians to degrees.
radiansToDegrees :: Floating a => a -> a
radiansToDegrees a = 180 * a / pi

-- | Converts an angle in degrees to radians.
degreesToRadians :: Floating a => a -> a
degreesToRadians rads = rads * pi / 180

-- | Rounds a floating-point number down to the closest integer.
floorFloating :: RealFrac a => a -> a
floorFloating = fromIntegral . floor

-- | Rounds a floating-point number.
roundFloating :: RealFrac a => a -> a
roundFloating = fromIntegral . round

-- | Rounds a floating-point number up to the closest integer.
ceilingFloating :: RealFrac a => a -> a
ceilingFloating = fromIntegral . ceiling

quotBy :: (Real a,Integral b) => a -> a -> b
quotBy d n = truncate ((toRational n) / (toRational d))

remBy :: Real a => a -> a -> a
remBy d n = n - (fromInteger f) * d where
    f = quotBy d n

-- | The dot product of two vectors.
dotProdVec :: Floating a => (a,a) -> (a,a) -> a
dotProdVec (x1, x2) (y1, y2)
        = x1 * y1 + x2 * y2

-- | The magniture of the cross product of two vectors
crossProdVec :: RealFloat a => (a,a) -> (a,a) -> a
crossProdVec v1 v2 = magnitudeVec v1 * magnitudeVec v2 * sin (abs $ angleVec v1 - angleVec v2)

-- | The magnitude of a vector.
magnitudeVec :: Floating a => (a,a) -> a
magnitudeVec (x,y) = sqrt (x * x + y * y)

-- | The angle of this vector, relative to the x-axis.
angleVec :: RealFloat a => (a,a) -> a
angleVec (x, y) = atan2 y x

scalarVec :: Floating a => a -> a -> (a,a)
scalarVec magnitude angle = (magnitude * cos angle,magnitude * sin angle)

distVec :: Floating a => (a,a) -> (a,a) -> a
distVec p1 p2 = abs $ magnitudeVec $ subVec p1 p2

addVec :: Floating a => (a,a) -> (a,a) -> (a,a)
addVec (x1,y1) (x2,y2) = (x1+x2,y1+y2)

extVec :: RealFloat a => (a,a) -> a -> (a,a)
extVec p m = scalarVec (magnitudeVec p + m) (angleVec p)

negVec :: Floating a => (a,a) -> (a,a)
negVec (x,y) = (-x,-y)

sumVec :: Floating a => [(a,a)] -> (a,a)
sumVec = foldr addVec (0,0)

averageVec :: Floating a => [(a,a)] -> (a,a)
averageVec vs = (average xs,average ys)
    where
    (xs,ys) = unzip vs

subVec :: Floating a => (a,a) -> (a,a) -> (a,a)
subVec (x1,y1) (x2,y2) = (x1-x2,y1-y2)

mulVec :: Floating a => a -> (a,a) -> (a,a)
mulVec s (x,y) = (s*x,s*y)

divVec :: Floating a => (a,a) -> a -> (a,a)
divVec (x,y) s = (x/s,y/s)

-- | creates a normal vector given an angle in radians
normVec :: Floating a => a -> (a,a)
normVec rads = scalarVec 1 rads

-- a line defined by two points
type DLine = (DPoint,DPoint)

-- a circle defined by a point and a radius
type DCircle = (DPoint,Double)

circleLineIntersection :: DCircle -> DLine -> [DPoint]
circleLineIntersection ((cx,cy),cr) ((x1,y1),(x2,y2)) =
    if (det < 0) then []
    else if (det == 0) then [(x1 + t * dx,y1 + t * dy)]
    else [(x1 + t1 * dx, y1 + t1 * dy),(x1 + t2 * dx, y1 + t2 * dy)]
  where
    dx = x2 - x1
    dy = y2 - y1
    a = dx^2 + dy^2
    b = 2 * (dx * (x1 - cx) + dy * (y1 - cy))
    c = (x1 - cx)^2 + (y1 - cy)^2 - cr^2
    det = b^2 - 4 * a * c
    -- 1 solution
    t = -b / (2 * a)
    -- 2 solutions
    t1 = (-b + sqrt(det)) / (2 * a)
    t2 = (-b - sqrt(det)) / (2 * a)

average :: Floating a => [a] -> a
average [] = error "average empty list"
average xs = sum xs / realToFrac (length xs)

(><) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
(f >< g) (a,b) = (f a,g b)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b

thr4 :: (a,b,c,d) -> c
thr4 (a,b,c,d) = c

fou4 :: (a,b,c,d) -> d
fou4 (a,b,c,d) = d



