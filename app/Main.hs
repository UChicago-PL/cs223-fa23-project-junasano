module Main where
import Diagrams.Prelude hiding (image,Angle, value)
import Diagrams.Backend.SVG.CmdLine (mainWith, B )
import Diagrams.TwoD()
import Data.Colour()
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
import Options.Applicative
import Data.Complex ( magnitude, Complex((:+)) )
import Diagrams.Backend.SVG (renderSVG)


-- For parsing we create a fractal datatype which includes the args
data Fractal
  = Snowflake Int
  | Sierpinski Int
  | Tree Int
  | Mandelbrot (Colour Double, Colour Double, Int, Int, (Double, Double), (Double, Double))


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
snowflakeTrail :: Int -> Trail V2 Double
snowflakeTrail n = k <> k # rotateBy (-1/3) <> k # rotateBy (1/3)
  where k = koch n

koch :: Int -> Trail V2 Double
koch 0 = fromOffsets [r2 (1, 0)]
koch n = k <> k # rotateBy (1/6) <> k # rotateBy (-1/6) <> k
  where k = koch (n-1) # scale (1/3)

-- Generate Koch Snowflake diagram
snowflake :: Int -> Diagram B
snowflake n = strokeTrail $ snowflakeTrail n


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

-- parser that contains the command (fractal name) and arguments for each fractal
fractal :: Parser Fractal
fractal = hsubparser
  ( command "snowflake" (info (Snowflake <$> option auto (long "iteration" <> short 'i' <> help "Snowflake iteration depth")) (progDesc "Generate a Koch snowflake"))
 <> command "sierpinski" (info (Sierpinski <$> option auto (long "iteration" <> short 'i' <> help "Sierpinski iteration depth")) (progDesc "Generate a Sierpinski triangle"))
 <> command "tree" (info (Tree <$> option auto (long "iteration" <> short 'i' <> help "Tree iteration depth")) (progDesc "Generate a tree"))
  )

renderFractal :: Fractal -> Diagram B
renderFractal (Snowflake n) = snowflake n
renderFractal (Sierpinski n) = sierpinski n
renderFractal (Tree n)       = tree n

main :: IO ()
main = do
    fractalOptions <- execParser (info (fractal <**> helper) fullDesc)
    let diagram = renderFractal fractalOptions
    renderSVG "output.svg" (dims $ V2 400 400) diagram

  