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
    putStrLn "Choose output type (still or animation):"
    hFlush stdout
    getLine

parseColor :: String -> Colour Double
parseColor str = fromMaybe black $ readColourName str

capitalize :: String -> String
capitalize [] = []
capitalize (head:tail) = toUpper head : map toLower tail

getFractalStill :: String -> (Int -> Int -> Fractal) -> IO Fractal
getFractalStill fractalName constructor = do
  putStrLn $ "Enter iterations for " ++ (capitalize fractalName) ++ ":"
  iter <- readLn
  return $ constructor iter iter

getFractalAnimate :: String -> (Int -> Int -> Fractal) -> IO Fractal
getFractalAnimate fractalName constructor = do
  putStrLn $ "Enter minimum iterations for " ++ (capitalize fractalName) ++ ":"
  minIter <- readLn
  putStrLn $ "Enter maximum iterations for " ++ (capitalize fractalName) ++ ":"
  constructor minIter <$> readLn

promptFractalArgs :: String -> String -> IO Fractal
promptFractalArgs fractalType outputType = do
  case capitalize fractalType of
    "Snowflake" -> case outputType of
        "still" -> getFractalStill fractalType Snowflake
        "animation" -> getFractalAnimate fractalType Snowflake
    "Dragon" -> case outputType of
        "still" -> getFractalStill fractalType Dragon
        "animation" -> getFractalAnimate fractalType Dragon
    "Sierpinski" -> case outputType of
        "still" -> getFractalStill fractalType Sierpinski
        "animation" -> getFractalAnimate fractalType Dragon
    "Tree" -> case outputType of
        "still" -> getFractalStill fractalType Tree
        "animation" -> getFractalAnimate fractalType Dragon
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

renderFractal :: Fractal -> Diagram B
renderFractal (Snowflake _ maxIter) = snowflake maxIter
renderFractal (Sierpinski _ maxIter) = sierpinski maxIter
renderFractal (Dragon _ maxIter) = dragonCurve maxIter
renderFractal (Tree _ maxIter) = pythagorasTree maxIter
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
