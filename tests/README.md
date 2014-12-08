To run parser & lexer tests, go to parent directory and run:

```
cabal test
```


To test sample output of Lexer, Parser, & IRifier from current directory:

``` 
../dist/build/Sensornet-Lexer/Sensornet-Lexer < test.txt
../dist/build/Sensornet-Parser/Sensornet-Parser < test.txt
../dist/build/Sensornet-IRifier/Sensornet-IRifier < test.txt 
```

To generate CFG-like flow diagram
```
../dist/build/Sensornet-CFGrapher/Sensornet-CFGrapher test.txt > test.dot
dot -Tpng test.dot > test.png
```

