module Parser where

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
import Data.Char (toLower, toUpper)
import FractalGenerators
import System.Process (callCommand)
import System.Directory
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)


getFractalType :: IO String 
getFractalType = do
  putStrLn "Choose a fractal! (type 'help' for options):"
  hFlush stdout
  getLine

showHelp :: IO ()
showHelp = do
  putStr "\ESC[2J"
  putStrLn "Available fractals: Snowflake, Sierpinski, Dragon, Tree, Mandelbrot"

showInvalidInput :: IO ()
showInvalidInput = do
  putStr "\ESC[2J"
  putStrLn "Invalid input"
  
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
      if outputType `elem` ["still", "animation"]
        then return outputType
        else do
          putStrLn "Invalid input. Please enter 'still' or 'animation'."
          outputTypeLoop


parseColor :: String -> Colour Double
parseColor colorString = fromMaybe black $ readColourName colorString

capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : map toLower t

getFractalStill :: String -> (Int -> Int -> Colour Double -> Fractal) -> IO Fractal
getFractalStill fractalName constructor = do
  putStrLn $ "Enter iterations for " ++ capitalize fractalName ++ ":"
  iter <- readLn
  putStrLn $ "Enter color (name) for " ++ capitalize fractalName ++ ":"
  col <- readLn
  return $ constructor iter iter col

getFractalAnimate :: String -> (Int -> Int -> Colour Double -> Fractal) -> IO Fractal
getFractalAnimate fractalName constructor = do
  putStrLn $ "Enter minimum iterations for " ++ capitalize fractalName ++ ":"
  minIter <- readLn
  putStrLn $ "Enter maximum iterations for " ++ capitalize fractalName ++ ":"
  maxIter <- readLn
  putStrLn $ "Enter color (name) for " ++ capitalize fractalName ++ ":"
  constructor minIter <$> pure maxIter <*> readLn

invalidOutputType :: String -> IO Fractal
invalidOutputType fractalType = do
  outputType <- promptOutputType
  promptFractalArgs fractalType outputType

getMandelbrotStill :: IO Fractal
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

  return $ Mandelbrot coolC warmC maxIter edge xRange yRange

        
promptFractalArgs :: String -> String -> IO Fractal
promptFractalArgs fractalType outputType = do
  case capitalize fractalType of
    "Snowflake" -> handleOutputType Snowflake
    "Dragon" -> handleOutputType Dragon
    "Sierpinski" -> handleOutputType Sierpinski
    "Tree" -> handleOutputType Tree
    "Mandelbrot" -> getMandelbrotStill
    _ -> error "Unsupported fractal type"
    where
    handleOutputType constructor = case outputType of
        "still" -> getFractalStill fractalType constructor
        "animation" -> getFractalAnimate fractalType constructor
        _ -> error "Invalid output type" 


generateFrames :: (Int -> Colour Double -> Diagram Rasterific) -> Int -> Int -> Colour Double -> FilePath -> IO [FilePath]
generateFrames constructor minIter maxIter tempDir c = forM [minIter..maxIter] $ \iter -> do
    let diagram = constructor iter c
    let fileName = tempDir ++ "/frame_" ++ show iter ++ ".png"
    renderRasterific fileName (dims $ V2 400 400) diagram
    return fileName

renderAnimation :: Fractal -> IO ()
renderAnimation fractal = do
    let tempDir = "temp_frames"
    createDirectoryIfMissing True tempDir

    fileNames <- case fractal of
        Snowflake minIter maxIter c -> generateFrames snowflake minIter maxIter c tempDir
        Sierpinski minIter maxIter c -> generateFrames sierpinski minIter maxIter c tempDir
        Dragon minIter maxIter c -> generateFrames dragonCurve minIter maxIter c tempDir
        Tree minIter maxIter c -> generateFrames pythagorasTree minIter maxIter c tempDir
        _ -> error "Unsupported fractal for animation"

    createGif fileNames
    cleanupFiles fileNames
    removeDirectoryRecursive tempDir

createGif :: [FilePath] -> IO ()
createGif fileNames = do
    let command = "convert -delay 40 -set dispose background -loop 0 " ++ unwords fileNames ++ " animation.gif"
    callCommand command
    cleanupFiles fileNames

cleanupFiles :: [FilePath] -> IO ()
cleanupFiles = mapM_ removeFile

renderStill :: Fractal -> IO ()
renderStill fractalType = do
    let diagram = renderFractal fractalType
    let fileName = "output.png"
    renderRasterific fileName (dims $ V2 400 400) diagram

renderFractal :: Fractal -> Diagram Rasterific
renderFractal (Snowflake _ maxIter colour) = snowflake maxIter colour
renderFractal (Sierpinski _ maxIter colour) = sierpinski maxIter colour
renderFractal (Dragon _ maxIter colour) = dragonCurve maxIter colour
renderFractal (Tree _ maxIter colour) = pythagorasTree maxIter colour
renderFractal (Mandelbrot coolC warmC maxIter edge (minX, maxX) (minY, maxY)) =
  mandelbrotGenerator coolC warmC maxIter edge (minX, maxX) (minY, maxY)

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
