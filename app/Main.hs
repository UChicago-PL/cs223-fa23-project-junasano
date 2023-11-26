module Main where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

triangleShape :: Diagram B
triangleShape = eqTriangle 1


sierpinski :: Int -> Diagram B
sierpinski 0 = triangleShape
sierpinski n = s
             ===
             (s ||| s)  # centerX
  where s = sierpinski (n-1) # scale (1/2)

main :: IO ()
main = mainWith (sierpinski 6 # centerXY # pad 1.1)

