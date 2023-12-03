module Main where
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
import System.IO ( hFlush, stdout )
import Control.Monad
import Data.Colour.Names (readColourName)
import Data.Maybe (fromMaybe)


-- For parsing we create a fractal datatype which includes the args
data Fractal
  = Snowflake { minIterations :: Int, maxIterations :: Int }
  | Sierpinski { minIterations :: Int, maxIterations :: Int }
  | Dragon { minIterations :: Int, maxIterations :: Int }
  | Tree { minIterations :: Int, maxIterations :: Int }
  | Mandelbrot { coolColor :: Colour Double
      , warmColor :: Colour Double
      , maxIterations :: Int
      , edgeSize :: Int
      , xRange :: (Double, Double)
      , yRange :: (Double, Double)
      }


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


-- Generate Heighway dragon fractal
dragon :: (Floating n, Ord n) => Trail V2 n -> Trail V2 n
dragon trail = (trail # rotateBy (-1/8) 
                    <> trail # rotateBy (5/8) # reverseTrail) 
                   # scale (1/sqrt 2)

-- Generate dragon fractal diagram
dragonCurve :: Int -> Diagram B
dragonCurve n = strokeTrail $ iterate dragon (hrule 1) !! n


-- Generate pythagoras tree
pythagorasTree :: Int -> Diagram B
pythagorasTree 0 = square 1 # translate (r2 (0, 1/2)) # lwG 0
pythagorasTree n =
  square 1          # translate (r2 (0, 1/2)) # lw thin
  `atop` branch     # translate (r2 (0,1)) # lwG 0
  `atop` pythagorasTree (n-1) # rotate (-asin 0.8 @@ rad) # scale 0.6 # translate (r2 ( 0.32,1.24))
  `atop` pythagorasTree (n-1) # rotate ( asin 0.6 @@ rad) # scale 0.8 # translate (r2 (-0.18,1.24))
  where
    branch = strokeLoop . fromVertices . map p2 $ [(0,0), (1,0), (0.8*0.8,0.8*0.6)]


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

        
        image = vcat $ map (hcat . map (toSquare . pixel)) grid

        
        toSquare :: Int -> Diagram B
        toSquare n = square 1 # lwG 0.01 # fc (blend normc coolC warmC)
            where
                normc = fromIntegral n / fromIntegral maxIter  





renderFractal :: Fractal -> Diagram B
renderFractal (Snowflake _ maxIter) = snowflake maxIter
renderFractal (Sierpinski _ maxIter) = sierpinski maxIter
renderFractal (Dragon _ maxIter) = dragonCurve maxIter
renderFractal (Tree _ maxIter) = pythagorasTree maxIter
renderFractal (Mandelbrot coolC warmC maxIter edge (minX, maxX) (minY, maxY)) =
  mandelbrotGenerator coolC warmC maxIter edge (minX, maxX) (minY, maxY)


getFractalType :: IO String 
getFractalType = do
  putStrLn "Choose a fractal! (type 'help' for options):"
  hFlush stdout
  getLine

showHelp :: IO ()
showHelp = putStrLn "Available fractals: Snowflake, Sierpinski, Dragon, Tree, Mandelbrot"

promptOutputType :: IO String
promptOutputType = do
    putStrLn "Choose output type (still or animation):"
    hFlush stdout
    getLine

parseColor :: String -> Colour Double
parseColor str = fromMaybe black $ readColourName str
  
promptFractalArgs :: String -> String -> IO Fractal
promptFractalArgs fractalType outputType = case fractalType of
    "Snowflake" -> case outputType of
        "still" -> do
            putStrLn "Enter iterations for Snowflake:"
            iter <- readLn
            return $ Snowflake iter iter
        "animation" -> do
            putStrLn "Enter minimum iterations for Snowflake:"
            minIter <- readLn
            putStrLn "Enter maximum iterations for Snowflake:"
            Snowflake minIter <$> readLn
    "Dragon" -> case outputType of
        "still" -> do
            putStrLn "Enter iterations for Dragon:"
            iter <- readLn
            return $ Dragon iter iter
        "animation" -> do
            putStrLn "Enter minimum iterations for Dragon:"
            minIter <- readLn
            putStrLn "Enter maximum iterations for Dragon:"
            Dragon minIter <$> readLn
    "Sierpinski" -> case outputType of
        "still" -> do
            putStrLn "Enter iterations for Sierpinski:"
            iter <- readLn
            return $ Sierpinski iter iter
        "animation" -> do
            putStrLn "Enter minimum iterations for Sierpinski:"
            minIter <- readLn
            putStrLn "Enter maximum iterations for Sierpinski:"
            Sierpinski minIter <$> readLn
    "Tree" -> case outputType of
        "still" -> do
            putStrLn "Enter iterations for Tree:"
            iter <- readLn
            return $ Tree iter iter
        "animation" -> do
            putStrLn "Enter minimum iterations for Tree:"
            minIter <- readLn
            putStrLn "Enter maximum iterations for Tree:"
            Tree minIter <$> readLn
    "Mandelbrot" -> do
        putStrLn "Enter max iterations (integer):"
        maxIter <- readLn

        putStrLn "Enter cool color (name):"
        coolStr <- getLine
        let coolC = parseColor coolStr

        putStrLn "Enter warm color (name):"
        warmStr <- getLine
        let warmC = parseColor warmStr

        putStrLn "Enter edge size (integer):"
        edge <- readLn

        putStrLn "Enter X range as (minX, maxX):"
        xRange <- readLn :: IO (Double, Double)

        putStrLn "Enter Y range as (minY, maxY):"
        yRange <- readLn :: IO (Double, Double)

        return $ Mandelbrot coolC warmC maxIter edge xRange yRange
    
      
renderAnimation :: Fractal -> IO ()
renderAnimation fractal = case fractal of
    Snowflake minIter maxIter -> 
        forM_ [minIter..maxIter] $ \iter -> do
            let diagram = snowflake iter
            let fileName = "snowflake_" ++ show iter ++ ".svg"
            renderSVG fileName (dims $ V2 400 400) diagram
    Sierpinski minIter maxIter -> 
        forM_ [minIter..maxIter] $ \iter -> do
            let diagram = sierpinski iter
            let fileName = "sierpinski_" ++ show iter ++ ".svg"
            renderSVG fileName (dims $ V2 400 400) diagram
    Dragon minIter maxIter -> 
        forM_ [minIter..maxIter] $ \iter -> do
            let diagram = dragonCurve iter
            let fileName = "dragon_" ++ show iter ++ ".svg"
            renderSVG fileName (dims $ V2 400 400) diagram
    Tree minIter maxIter -> 
        forM_ [minIter..maxIter] $ \iter -> do
            let diagram = pythagorasTree iter
            let fileName = "tree_" ++ show iter ++ ".svg"
            renderSVG fileName (dims $ V2 400 400) diagram

renderIteration :: Fractal -> IO ()
renderIteration fractalType = do
    let diagram = renderFractal fractalType
    let fileName = "output.svg"
    renderSVG fileName (dims $ V2 400 400) diagram

promptFractalTypeLoop :: IO String
promptFractalTypeLoop = do
    fractalType <- getFractalType
    if fractalType == "help" then do
        showHelp
        promptFractalTypeLoop
    else 
        return fractalType


main :: IO ()
main = do
    fractalType <- promptFractalTypeLoop
    outputType <- promptOutputType

    fractal <- promptFractalArgs fractalType outputType
    case outputType of
        "still" -> renderIteration fractal
        "animation" -> renderAnimation fractal
        _ -> putStrLn "Invalid output type"



