module Parser where

import Diagrams.Prelude hiding ( image,Angle, value, frame, index )
import Diagrams.TwoD()
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE TypeFamilies #-}
import Diagrams.Animation()
import System.IO ( hFlush, stdout )
import Control.Monad
import Data.Maybe ( fromMaybe )
import Data.Char ( toLower, toUpper )
import System.Process ( callCommand )
import System.Directory
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import FractalGenerators


type ZoomRange = ((Double, Double), (Double, Double))

-- function to prompt user for fractal type
getFractalType :: IO String 
getFractalType = do
  putStrLn "Choose a fractal! (type 'help' for options):"
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
      putStrLn "Choose output type (still or animation):"
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
parseColor :: String -> Colour Double
parseColor colorString = fromMaybe black $ readColourName colorString

-- capitalize first letter of string
capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : map toLower t

-- function to prompt user for still fractal (except for mandelbrot) arguments
getFractalStill :: String -> (Int -> Int -> Colour Double -> Fractal) -> IO (Fractal, Colour Double)
getFractalStill fractalName constructor = do
  putStrLn $ "Enter iterations for " ++ capitalize fractalName ++ ":"
  iter <- readLn
  putStrLn $ "Enter color (name) for " ++ capitalize fractalName ++ ":"
  colorName <- getLine
  let col = parseColor colorName
  putStrLn $ "Enter background color (name) for " ++ capitalize fractalName ++ ":"
  bgColorName <- getLine
  let bgColor = parseColor bgColorName

  return (constructor iter iter col, bgColor)

-- function to prompt user for animation fractal (except for mandelbrot) arguments  
getFractalAnimate :: String -> (Int -> Int -> Colour Double -> Fractal) -> IO (Fractal, Colour Double)
getFractalAnimate fractalName constructor = do
  putStrLn $ "Enter minimum iterations for " ++ capitalize fractalName ++ ":"
  minIter <- readLn
  putStrLn $ "Enter maximum iterations for " ++ capitalize fractalName ++ ":"
  maxIter <- readLn
  putStrLn $ "Enter color (name) for " ++ capitalize fractalName ++ ":"
  colorName <- getLine
  let col = parseColor colorName
  putStrLn $ "Enter background color (name) for " ++ capitalize fractalName ++ ":"
  bgColorName <- getLine
  let bgColor = parseColor bgColorName
  return (constructor minIter maxIter col, bgColor)

-- function to prompt user for still mandelbrot arguments
getMandelbrotStill :: IO (Fractal, Colour Double)
getMandelbrotStill = do
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

  return (Mandelbrot coolC warmC maxIter edge xRange yRange, white)

-- function to prompt user for animation mandelbrot arguments
getMandelbrotZoom :: IO ([Fractal], Colour Double)
getMandelbrotZoom = do
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

  putStrLn "Enter start X range as (minX, maxX):"
  startXRange <- readLn :: IO (Double, Double)

  putStrLn "Enter end X range as (minX, maxX):"
  endXRange <- readLn :: IO (Double, Double)

  putStrLn "Enter start Y range as (minY, maxY):"
  startYRange <- readLn :: IO (Double, Double)
  
  putStrLn "Enter end Y range as (minY, maxY):"
  endYRange <- readLn :: IO (Double, Double)
  
  putStrLn "Enter number of frames for animation:"
  numFrames <- readLn

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
    removeDirectoryRecursive tempDir

-- converts png files into one gif
createGif :: [FilePath] -> IO ()
createGif fileNames = do
    let command = "convert -delay 40 -set dispose background -loop 0 " ++ unwords fileNames ++ " animation.gif"
    callCommand command
    cleanupFiles fileNames

-- for removing temp png files that are created when making animation
cleanupFiles :: [FilePath] -> IO ()
cleanupFiles = mapM_ removeFile

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
