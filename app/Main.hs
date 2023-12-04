module Main where

import System.IO ( hFlush, hSetBuffering, stdin, stdout, BufferMode(LineBuffering)  )
import Parser


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    putStr "\ESC[2J"
    hFlush stdout
    fractalType <- promptFractalTypeLoop
    outputType <- promptOutputType
    if fractalType == "mandelbrot" && outputType == "animation" then do
        (fractals, bgC) <- getMandelbrotZoom
        renderMandelbrotAnimation bgC fractals
    else do
        (fractal, bgC) <- promptFractalArgs fractalType outputType
        case outputType of
            "still" -> renderStill bgC fractal
            "animation" -> renderAnimation bgC fractal
            _ -> putStrLn outputType