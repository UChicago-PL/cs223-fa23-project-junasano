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
    if capitalize fractalType == "Mandelbrot" && capitalize outputType == "Animation" then do
        (fractal, bgC) <- getMandelbrotZoom
        renderMandelbrotAnimation bgC fractal
        putStrLn "Animation created! (animation.gif)"
    else if capitalize fractalType == "Mandelbrot" && capitalize outputType == "Still" then do
        (fractal, bgC) <- getMandelbrotStill
        renderStill bgC fractal
        putStrLn "Still image created! (output.png)"
    else do
        (fractal, bgC) <- promptFractalArgs fractalType outputType
        case capitalize outputType of
            "Still" -> do 
                renderStill bgC fractal
                putStrLn "Still image created! (output.png)"
            "Animation" -> do 
                renderAnimation bgC fractal
                putStrLn "animation created! (animation.gif)"
            _ -> error "Unsupported output type"