module Main where
import Diagrams.Prelude hiding (image,Angle)
import Diagrams.Backend.SVG.CmdLine ( mainWith, B )
import Diagrams.TwoD
import Data.Colour (blend)
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Complex ( magnitude, Complex((:+)) )

triangleShape :: Diagram B
triangleShape = eqTriangle 1

-- Generate sierpinski triangles
sierpinski :: Int -> Diagram B
sierpinski 0 = triangleShape
sierpinski n = s
             ===
             (s ||| s)  # centerX
  where s = sierpinski (n-1) # scale (1/2)


-- Generate Koch Snowflake
kochSnowFlake :: Int -> Diagram B
kochSnowFlake 0 = triangleShape
kochSnowFlake n = 
  let tri1 = eqTriangle 1 # rotateBy (1/6) -- # scale (1/3)
  in
    (tri1 `atop` eqTriangle 1 # translate (r2 (0,0)))-- === tri1
    --beside (r2 (4,(1/6))) tri1 (eqTriangle 1 === tri1) # center
  --( tri1 ||| (eqTriangle 1 === tri1) ||| tri1)  # center

tree :: Int -> Diagram B
tree 1 = circle 1.25
                    # translate (r2 (0, 1/2)) # lwG 0
tree n =
  square 1          # translate (r2 (0, 1/2)) 
                     # lw thin
  `atop` triangle   # translate (r2 (0,1)) # lwG 0
  `atop` tree (n-1) # rotate (-asin 0.8 @@ rad)
                    # scale 0.6 # translate (r2 ( 0.32,1.24))
  `atop` tree (n-1) # rotate ( asin 0.6 @@ rad)
                    # scale 0.8 # translate (r2 (-0.18,1.24))
  where
    triangle = translate (r2 (-0.5,0)) . strokeLoop . closeLine
                 . fromVertices . map p2 $ [(0,0), (1,0), (0.8*0.8,0.8*0.6)]


-- Generate the mandelbrot set as a grid, specifying color gradients, max iterations, 
-- size of the edge, range of the real vals, and range of imaginary vals
mandelbrotGenerator :: Colour Double -> Colour Double -> Int -> Int -> (Double, Double) -> (Double, Double) -> Diagram B
mandelbrotGenerator coolC warmC maxIter edge (minX, maxX) (minY, maxY)= image # bgFrame 5 white
    where
        quadratic c z = z*z + c
        critical_orbit c = iterate (quadratic c) 0
        pixel c = length . takeWhile (\z -> magnitude z <= 2) . take maxIter $ critical_orbit c
        side n v0 v1 =
            let sv = (v1 - v0) / fromIntegral n
            in  [v0, (v0 + sv) .. v1]

        
        sideX = side edge minX maxX
        sideY = side edge minY maxY
        grid = [ [x :+ y | x <- sideX] | y <- sideY ]

        
        image = vcat . map hcat $ map (map (toSquare . pixel)) grid

        
        toSquare :: Int -> Diagram B
        toSquare n = square 1 # lwG 0.01 # fc (blend normc coolC warmC)
            where
                normc = fromIntegral n / fromIntegral maxIter  


-- Main function to generate the SVG file
main :: IO ()
main = mainWith $ tree 3
  --mainWith $ kochSnowFlake 2
  --mainWith (mandelbrotGenerator yellow red 100 200 (-2.0, 1.0) (-1.0, 1.0))