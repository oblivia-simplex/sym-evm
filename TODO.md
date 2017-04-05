# TODO

  1. Refactor to use lenses? -- DONE
  2. Figure out right semantics... I don't like the types yet
    * In particular, i'd like semantics to be similar to the small-step error
      semantics in DVH PL notes. Flat structure makes this difficult.
  3. Convert to monadic interpreter?
  4. Rolling my own Set data structure was dumb, should just use Data.Set for now. Will probably need
     to change to provide heuristics (e.g. priority queue rather than set) but we can do that later. -- DONE
