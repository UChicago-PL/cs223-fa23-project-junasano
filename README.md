# Fractal Generator in Haskall:

We are going to use this readme to document our progress as we work through the final project, and to help each other out with setup and execution.

## Starting out:

In order to run this project, I have created a .cabal file with the necessary build dependencies. To make sure this project runs on your computer, make sure to run:

`cabal update`
`cabal install --dependencies-only`
`cabal build`

You should then be ready to go with running the project. Keep in mind I have placed the haskall files in a folder called "app".

When you want to run Main.hs in order to get an output (like an image), you will not be able to just run `cabal run`. Instead, you must run something like the following: 
`cabal run -- cs223-fa23-project-junasano -o output.svg -w 400`
this specifies the name of the project to run, the -o flag indicates the output file, and the -w flag indicates the width of the output image. After running a command like this, you should see the output file in the directory.

You can specify other flags, I recommend checking out the following link which I have been using the get started: https://diagrams.github.io/doc/quickstart.html.


## UPDATE ON COMMAND LINE PARSER:
I created a basic parser for snowflake, sierpinski, and tree fractals (havent gotten to the mandelbrot one yet bc its more complicated). you can now run the program with a command like the following:
cabal run -- cs223-fa23-project-junasano [fractalname] -i [number of iterations]
for instance:
cabal run -- cs223-fa23-project-junasano tree -i 6

Do do this I got rid of the mainWith function and I am using Options.Applicative along with the renderSVG function from Diagrams.Backend.SVG.

We now need to make this work for the mandelbrot fracal which requires more inputs and also support animation
