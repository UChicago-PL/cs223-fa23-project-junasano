module Main where
import Diagrams.Prelude hiding (image)
import Diagrams.Backend.SVG.CmdLine
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Complex

triangleShape :: Diagram B
triangleShape = eqTriangle 1

-- Generate sierpinski triangles
sierpinski :: Int -> Diagram B
sierpinski 0 = triangleShape
sierpinski n = s
             ===
             (s ||| s)  # centerX
  where s = sierpinski (n-1) # scale (1/2)

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
main = mainWith (mandelbrotGenerator yellow red 100 200 (-2.0, 1.0) (-1.0, 1.0))