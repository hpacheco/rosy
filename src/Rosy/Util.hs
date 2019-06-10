module Rosy.Util where

radiansToDegrees :: Float -> Float
radiansToDegrees a = 180 * a / pi

degreesToRadians :: Float -> Float
degreesToRadians rads = rads * pi / 180