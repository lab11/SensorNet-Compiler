Tests
-----

To run parser & lexer tests, go to parent directory and run:

    cabal test


Test Sample Code
----------------

To test output of Lexer, Parser, & IRifier on sample code (like `test.txt`), run from current directory:

    ../dist/build/Sensornet-Lexer/Sensornet-Lexer < test.txt
    ../dist/build/Sensornet-Parser/Sensornet-Parser < test.txt
    ../dist/build/Sensornet-IRifier/Sensornet-IRifier < test.txt 
    
To perform analyses on the sample code:    
    
    ../dist/build/Sensornet-TypeInferer/Sensornet-TypeInferer [header file] test.txt
    ../dist/build/Sensornet-ProfusionChecker/Sensornet-ProfusionChecker test.txt
    ./dist/build/Sensornet-CFGrapher/Sensornet-CFGrapher test.txt

To generate CFG-like flow diagram using GraphViz (good to compare with output of IRifier):

    ../dist/build/Sensornet-CFGrapher/Sensornet-CFGrapher test.txt > test.dot
    dot -Tpng test.dot > test.png


