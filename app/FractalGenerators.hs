module FractalGenerators where
  
import Diagrams.Prelude hiding (image,Angle, value)
import Diagrams.Backend.SVG.CmdLine (mainWith, B )
import Diagrams.TwoD()
import Text.Read (readMaybe)
import Data.Colour( Colour )
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
import Options.Applicative
import Data.Complex ( magnitude, Complex((:+)) )
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Animation()
import System.IO ( hFlush, hSetBuffering, stdin, stdout, BufferMode(LineBuffering)  )
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)


-- For parsing we create a fractal datatype which includes the args
data Fractal
  = Snowflake { minIterations :: Int, maxIterations :: Int, color :: Colour Double }
  | Sierpinski { minIterations :: Int, maxIterations :: Int, color :: Colour Double }
  | Dragon { minIterations :: Int, maxIterations :: Int, color :: Colour Double }
  | Tree { minIterations :: Int, maxIterations :: Int, color :: Colour Double }
  | Mandelbrot { coolColor :: Colour Double
      , warmColor :: Colour Double
      , maxIterations :: Int
      , edgeSize :: Int
      , xRange :: (Double, Double)
      , yRange :: (Double, Double)
      }


triangleShape :: Diagram Rasterific
triangleShape = eqTriangle 1

-- Generate sierpinski triangles
sierpinski :: Int -> Colour Double -> Diagram Rasterific
sierpinski 0 c = triangleShape # fc c
sierpinski n c = s
             ===
             (s ||| s)  # centerX
  where s = sierpinski (n-1) # scale (1/2) # fc c


-- Generate Koch Snowflake
snowflakeTrail :: Int -> Colour Double -> Trail V2 Double
snowflakeTrail n c = k <> k # rotateBy (-1/3) <> k # rotateBy (1/3)
  where k = koch n c

koch :: Int -> Colour Double -> Trail V2 Double
koch 0 c = fromOffsets [r2 (1, 0)]
koch n c = k <> k # rotateBy (1/6) <> k # rotateBy (-1/6) <> k 
  where k = koch (n-1) # scale (1/3) # fc c

-- Generate Koch Snowflake diagram
snowflake :: Int -> Colour Double -> Diagram Rasterific
snowflake n c = strokeTrail $ snowflakeTrail n c


-- Generate Heighway dragon fractal
dragon :: (Floating n, Ord n) => Trail V2 n -> Colour Double -> Trail V2 n
dragon trail c = (trail # rotateBy (-1/8)
                    <> trail # rotateBy (5/8) # reverseTrail) 
                   # scale (1/sqrt 2) # fc c

-- Generate dragon fractal diagram
dragonCurve :: Int -> Colour Double -> Diagram Rasterific
dragonCurve n c = strokeTrail $ iterate (dragon (hrule 1) c) !! n


-- Generate pythagoras tree
pythagorasTree :: Int -> Colour Double -> Diagram Rasterific
pythagorasTree 0 c = square 1 # translate (r2 (0, 1 / 2)) # lwG 0 # fc c
pythagorasTree n c =
  square 1          # translate (r2 (0, 1/2)) # lw thin # fc c
  `atop` branch     # translate (r2 (0,1)) # lwG 0 # fc c
  `atop` pythagorasTree (n-1) # rotate (-asin 0.8 @@ rad) # scale 0.6 # translate (r2 ( 0.32,1.24)) # fc c
  `atop` pythagorasTree (n-1) # rotate ( asin 0.6 @@ rad) # scale 0.8 # translate (r2 (-0.18,1.24)) # fc c
  where
    branch = strokeLoop . fromVertices . map p2 $ [(0,0), (1,0), (0.8*0.8,0.8*0.6)]


-- Generate the mandelbrot set as a grid, specifying color gradients, max iterations, 
-- size of the edge, range of the real vals, and range of imaginary vals
mandelbrotGenerator :: Colour Double -> Colour Double -> Int -> Int -> (Double, Double) -> (Double, Double) -> Diagram Rasterific
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

        
        image = vcat $ map (hcat . map (toSquare . pixel)) grid

        
        toSquare :: Int -> Diagram Rasterific
        toSquare n = square 1 # lwG 0.01 # fc (blend normc coolC warmC)
            where
                normc = fromIntegral n / fromIntegral maxIter  