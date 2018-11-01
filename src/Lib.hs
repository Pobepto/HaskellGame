module Lib ( 
  someFunc
) where

import CodeWorld

someFunc :: IO ()
someFunc = drawingOf draw

draw :: Picture
draw = colored black (solidRectangle 1 1)