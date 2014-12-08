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


To generate CFG-like flow diagram (good to compare with output of IRifier):

    ../dist/build/Sensornet-CFGrapher/Sensornet-CFGrapher test.txt > test.dot
    dot -Tpng test.dot > test.png


