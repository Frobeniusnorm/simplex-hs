# simplex-hs
Simple implementations of various variants of the simplex algorithm in Haskell with [hmatrix](https://hackage.haskell.org/package/hmatrix-0.20.2).

#### Many implementations (i.e. all) are horribly inefficient, this project is just a "learning + hobby" project. Read if you are interested in the implementation in haskell, but do not use in anything serious! ##

- Version 1 (`Simplex1.hs`) implements the informal version of the Simplex algorithm
- Version 2 (`Simplex2.hs`) implements the basic version of the Simplex algorithm without any optimizations
- Version 3 (`Simplex3.hs`) implements a extensible version for different pricing and ratio test methods on top of the second version (several methods are implemented on `Selection.hs`)

The Main class allows passing of problem descriptions or files containing such. Examples can be found in `/examples`.

#### Building
no build tool needed, just plain ghc:
```
ghc -o simplex -O3 Main.hs
```

#### Dependencies
- [https://hackage.haskell.org/package/hmatrix](hmatrix)
