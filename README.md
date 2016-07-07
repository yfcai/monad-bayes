A library for probabilistic programming in Haskell using probability monads. The emphasis is on composition of inference algorithms implemented in terms of monad transformers. The code is still experimental, but will be released on Hackage as soon as it reaches relative stability. User's guide will appear soon. In the meantime see the `models` folder that contains several examples.

## Compilation

Make sure the Haskell build tool `stack` is installed. We tested with stack version 1.0.2. Compilation probably needs an internet connection in order to download dependencies.

* `stack build` compiles source files.
* `stack test` runs the test suite `/test/Spec.hs`.

## Execution

The easiest way to try the library out is `stack ghci`. It launches a Haskell repl and imports all our modules. Below is a script you can paste into a `stack ghci` session. It tries out the example code in the introduction.

```
-- make prompt smaller
:set prompt "? "

-- the weather model in one line
let weather = do { rain <- bernoulli 0.3; when rain (factor 0.2); sprinkler <- bernoulli (if rain then 0.1 else 0.4); when sprinkler (factor 0.1); return rain }

-- exact posterior of weather by exhaustive numeration
-- expected output: [(False,0.448),(True,5.46e-2)]
enumerate weather

-- get 3 samples using SMC
runPopulation $ smc 3 2 weather :: IO [(Bool, LogFloat)]

-- compute exact posterior of weather model transformed by SMC
-- running with 2 particles instead of 3; it will be slow otherwise.
-- verify visually that the output is close to `enumerate weather`.
enumerate $ collapse $ smc 2 2 weather
```

