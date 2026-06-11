# symbolic

It can be built and run with [stack](https://docs.haskellstack.org/en/stable/README/):

```
stack build
stack exec symbolic
```

Example Python input programs are stored in `examples/python/`.

Run one example with Cabal:

```
cabal run symbolic -- examples/python/test_01_echo.py
```

Run all examples:

```
./examples/python/run_all_examples.sh
```
