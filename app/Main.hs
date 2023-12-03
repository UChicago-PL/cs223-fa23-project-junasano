module Main where

import System.IO ( hFlush, hSetBuffering, stdin, stdout, BufferMode(LineBuffering)  )
import Parser
import FractalGenerators


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    putStr "\ESC[2J"
    hFlush stdout
    fractalType <- promptFractalTypeLoop
    outputType <- promptOutputType

    fractal <- promptFractalArgs fractalType outputType
    case outputType of
        "still" -> renderIteration fractal
        "animation" -> renderAnimation fractal
        _ -> putStrLn outputType