module Spec
        (test
        ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, red)

test :: IO ()
test = display (InWindow "Hello World" (500,500)(100,100))
               (makeColor 0.9 0.9 0.9 1)
               (Pictures [Color red (Circle 1000)
                         , Text "Hello World Text"
                         ])


