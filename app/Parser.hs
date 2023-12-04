module Parser where

import Diagrams.Prelude hiding ( image,Angle, value, frame, index )
import Diagrams.TwoD()
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE TypeFamilies #-}
import Diagrams.Animation()
import System.IO ( hFlush, stdout )
import Control.Monad
import Data.Maybe ( fromMaybe )
import Data.Char ( toLower, toUpper, isLetter )
import System.Process ( callCommand )
import System.Directory
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import FractalGenerators


type ZoomRange = ((Double, Double), (Double, Double))

-- function to prompt user for fractal type
getFractalType :: IO String 
getFractalType = do
  putStr "Choose a fractal! (type 'help' for options):"
  enterString
  hFlush stdout
  getLine

-- function to show available fractals when user types 'help'
showHelp :: IO ()
showHelp = do
  putStr "\ESC[2J"
  putStrLn "Available fractals: Snowflake, Sierpinski, Dragon, Tree, Mandelbrot"

-- function to show invalid input when user types an invalid fractal
showInvalidInput :: IO ()
showInvalidInput = do
  putStr "\ESC[2J"
  putStrLn "Invalid input"
  
-- function to prompt user for output type. Only accepts still or animation
promptOutputType :: IO String
promptOutputType = do
    putStr "\ESC[2J"
    outputTypeLoop
  where
    outputTypeLoop :: IO String
    outputTypeLoop = do
      putStr "Choose output type (still or animation):"
      enterString
      hFlush stdout
      outputType <- getLine
      if capitalize outputType `elem` ["Still", "Animation"]
        then return outputType
        else do
          putStr "\ESC[2J"
          putStrLn "Invalid output type. Please enter 'still' or 'animation'."
          outputTypeLoop

-- function to prompt user for fractal type. Only accepts valid fractal types
promptFractalTypeLoop :: IO String
promptFractalTypeLoop = do
    fractalType <- getFractalType
    let fractalTypeLower = map toLower fractalType
    if fractalTypeLower == "help" then do
        showHelp
        promptFractalTypeLoop
    else if fractalTypeLower `elem` ["snowflake", "dragon", "sierpinski", "tree", "mandelbrot"] then
        return fractalType
    else do
        showInvalidInput
        promptFractalTypeLoop

-- converts color name to color double
parseColor :: String -> Colour Double -> Colour Double
parseColor colorString defaultColor = fromMaybe defaultColor $ readColourName colorString

enterString :: IO ()
enterString = putStr "\n> "

-- capitalize first letter of string
capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : map toLower t

promptWithDefault :: Read a => String -> a -> IO a
promptWithDefault message defaultValue = do
    putStr (message ++ "\n> ")
    hFlush stdout
    input <- getLine
    case reads input of
        [(value, "")] -> return value 
        _ -> return defaultValue
       
promptWithDefaultString :: String -> String -> IO String
promptWithDefaultString message defaultValue = do
    putStr (message ++ "\n> ")
    hFlush stdout
    input <- getLine
    if all isLetter input then return input else return defaultValue

-- function to prompt user for still fractal (except for mandelbrot) arguments
getFractalStill :: String -> (Int -> Int -> Colour Double -> Fractal) -> IO (Fractal, Colour Double)
getFractalStill fractalName constructor = do
  iter <- promptWithDefault ("Enter iterations for " ++ capitalize fractalName ++ " (default: 5)") (5 :: Int)
  colorName <- promptWithDefaultString ("Enter color (name) for " ++ capitalize fractalName ++ " (default: blue)") "blue"
  bgColorName <- promptWithDefaultString ("Enter background color (name) for " ++ capitalize fractalName ++ " (default: red)") "red"

  let col = parseColor colorName blue
  let bgColor = parseColor bgColorName red
  return (constructor iter iter col, bgColor)

-- function to prompt user for animation fractal (except for mandelbrot) arguments  
getFractalAnimate :: String -> (Int -> Int -> Colour Double -> Fractal) -> IO (Fractal, Colour Double)
getFractalAnimate fractalName constructor = do
  minIter <- promptWithDefault ("Enter start iteration for " ++ capitalize fractalName ++ " (default: 1)") (1 :: Int)
  maxIter <- promptWithDefault ("Enter end iteration for " ++ capitalize fractalName ++ " (default: 6)") (6 :: Int)
  colorName <- promptWithDefaultString ("Enter color (name) for " ++ capitalize fractalName ++ " (default: blue)") "blue"
  bgColorName <- promptWithDefaultString ("Enter background color (name) for " ++ capitalize fractalName ++ " (default: red)") "red"

  let col = parseColor colorName blue
  let bgColor = parseColor bgColorName red
  return (constructor minIter maxIter col, bgColor)
  

-- function to prompt user for still mandelbrot arguments
getMandelbrotStill :: IO (Fractal, Colour Double)
getMandelbrotStill = do
  maxIter <- promptWithDefault "Enter number of iterations (int) (default: 100)" (100 :: Int)
  coolStr <- promptWithDefaultString "Enter cool color (name) (default: blue)" "blue"
  warmStr <- promptWithDefaultString "Enter warm color (name) (default: red)" "red"
  edge <- promptWithDefault "Enter number of pixels per edge (int) (default: 100)" (100 :: Int)
  xRange <- promptWithDefault "Enter X range as (minX, maxX) (default: (-2, 1))" (-2 :: Double, 1 :: Double)
  yRange <- promptWithDefault "Enter y range as (minY, maxY) (default: (-1.5, 1.5))" (-1.5 :: Double, 1.5 :: Double)
  
  let warmC = parseColor warmStr blue
  let coolC = parseColor coolStr red
  return (Mandelbrot coolC warmC maxIter edge xRange yRange, white)
  

-- function to prompt user for animation mandelbrot arguments
getMandelbrotZoom :: IO ([Fractal], Colour Double)
getMandelbrotZoom = do
  maxIter <- promptWithDefault "Enter start number of iterations (int) (default: 100)" (100 :: Int)
  coolStr <- promptWithDefaultString "Enter cool color (name) (default: blue)" "blue"
  warmStr <- promptWithDefaultString "Enter warm color (name) (default: red)" "red"
  edge <- promptWithDefault "Enter number of pixels per edge (int) (default: 100)" (100 :: Int)
  startXRange <- promptWithDefault "Enter start X range as (minX, maxX) (default: (-2, 1))" (-2 :: Double, 1 :: Double)
  endXRange <- promptWithDefault "Enter end X range as (minX, maxX) (default: (-0.751, -0.749))" (-0.751 :: Double, -0.749 :: Double)
  startYRange <- promptWithDefault "Enter start y range as (minY, maxY) (default: (-1.5, 1.5))" (-1.5 :: Double, 1.5 :: Double)
  endYRange <- promptWithDefault "Enter end y range as (minY, maxY) (default: (0.099, 0.101))" (0.099 :: Double, 0.101 :: Double)
  numFrames <- promptWithDefault "Enter total number of frames (default 10)" (10 :: Int)

  let coolC = parseColor coolStr blue
  let warmC = parseColor warmStr red
  let xRanges = interpolate numFrames startXRange endXRange
  let yRanges = interpolate numFrames startYRange endYRange
  let fractals = [Mandelbrot coolC warmC (maxIter + 50 * i) edge x y | 
                  ((x, y), i) <- zip (zip xRanges yRanges) [0..]]
  return (fractals, white) 

-- function to interpolate between two ranges
interpolate :: Int -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
interpolate numFrames (startMin, startMax) (endMin, endMax) = 
  [(startMin + (endMin - startMin) * progress, 
     startMax + (endMax - startMax) * progress) 
    | frame <- [0..numFrames - 1], 
      let progress = fromIntegral frame / fromIntegral (numFrames - 1)]

-- changes processes based on different fractals
promptFractalArgs :: String -> String -> IO (Fractal, Colour Double)
promptFractalArgs fractalType outputType = do
  case capitalize fractalType of
    "Snowflake" -> handleOutputType Snowflake
    "Dragon" -> handleOutputType Dragon
    "Sierpinski" -> handleOutputType Sierpinski
    "Tree" -> handleOutputType Tree
    _ -> error "Unsupported fractal type"
    where
    handleOutputType constructor = case capitalize outputType of
        "Still" -> getFractalStill fractalType constructor
        "Animation" -> getFractalAnimate fractalType constructor
        _ -> error "Invalid output type" 
    
-- function to generate frames for animation
generateFrames :: Colour Double -> (Int -> Colour Double -> Diagram Rasterific) -> Int -> Int -> Colour Double -> FilePath -> IO [FilePath]
generateFrames bgC constructor minIter maxIter c tempDir = forM [minIter..maxIter] $ \iter -> do
    let diagram = constructor iter c # bg bgC
    let fileName = tempDir ++ "/frame_" ++ show iter ++ ".png"
    renderRasterific fileName (dims $ V2 400 400) diagram
    return fileName

-- function to generate frames for mandelbrot animation
generateMandelbrotFrames :: Colour Double -> [Fractal] -> FilePath -> IO [FilePath]
generateMandelbrotFrames bgC fractals tempDir = forM (zip [(0 :: Int)..] fractals) $ \(index, fractal) -> do
    let diagram = renderFractal fractal # bg bgC
    let fileName = tempDir ++ "/frame_" ++ show index ++ ".png"
    renderRasterific fileName (dims $ V2 400 400) diagram
    return fileName

-- function to render animation for non-mandelbrot fractals
renderAnimation :: Colour Double -> Fractal -> IO ()
renderAnimation bgC fractal = do
    let tempDir = "temp_frames"
    createDirectoryIfMissing True tempDir

    fileNames <- case fractal of
        Snowflake minIter maxIter c -> generateFrames bgC snowflake minIter maxIter c tempDir
        Sierpinski minIter maxIter c -> generateFrames bgC sierpinski minIter maxIter c tempDir
        Dragon minIter maxIter c -> generateFrames bgC dragonCurve minIter maxIter c tempDir
        Tree minIter maxIter c -> generateFrames bgC pythagorasTree minIter maxIter c tempDir
        _ -> error "Unsupported fractal for animation"

    createGif fileNames
    cleanupFiles fileNames
    removeDirectoryRecursive tempDir

-- function to render mandelbrot animation
renderMandelbrotAnimation :: Colour Double -> [Fractal] -> IO ()
renderMandelbrotAnimation bgC fractals = do
    let tempDir = "temp_frames"
    createDirectoryIfMissing True tempDir
    fileNames <- generateMandelbrotFrames bgC fractals tempDir
    createGif fileNames
    cleanupFiles fileNames

-- converts png files into one gif using the ImageMagick command, "convert"
createGif :: [FilePath] -> IO ()
createGif fileNames = do
    let command = "convert -delay 40 -set dispose background -loop 0 " ++ unwords fileNames ++ " animation.gif"
    callCommand command
    cleanupFiles fileNames

-- for removing temp png files that are created when making animation
cleanupFiles :: [FilePath] -> IO ()
cleanupFiles fileNames = forM_ fileNames $ \fileName -> do
    fileExists <- doesFileExist fileName
    when fileExists $ removeFile fileName


-- function to create still 
renderStill :: Colour Double -> Fractal -> IO ()
renderStill bgCol fractalType = do
    let diagram = renderFractal fractalType # bg bgCol
    let fileName = "output.png"
    renderRasterific fileName (dims $ V2 400 400) diagram

-- starts the functions in fractalgenerator module to create the actual fractals
renderFractal :: Fractal -> Diagram Rasterific
renderFractal (Snowflake _ maxIter colour) = snowflake maxIter colour
renderFractal (Sierpinski _ maxIter colour) = sierpinski maxIter colour
renderFractal (Dragon _ maxIter colour) = dragonCurve maxIter colour
renderFractal (Tree _ maxIter colour) = pythagorasTree maxIter colour
renderFractal (Mandelbrot coolC warmC maxIter edge (minX, maxX) (minY, maxY)) =
  mandelbrotGenerator coolC warmC maxIter edge (minX, maxX) (minY, maxY)
