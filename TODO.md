# TODO


# BACKBURNER

  1. Figure out right semantics... I don't like the types yet
    * In particular, i'd like semantics to be similar to the small-step error
      semantics in DVH PL notes. Flat structure makes this difficult.
  2. Convert to monadic interpreter?

# DONE

  1. Refactor to use lenses? -- DONE
  2. Rolling my own Set data structure was dumb, should just use Data.Set for now. Will probably need
     to change to provide heuristics (e.g. priority queue rather than set) but we can do that later. -- DONE
  3. Convert to use H halting function from yellow paper
  4. Collect final states as well as errors

## EtherPot Instruction Counts

  SHA3 31

  CALLDATASIZE 1
  CALLDATALOAD 17

  SSTORE 8
  SLOAD 25

  MSTORE 77
  MLOAD 32

  JUMP 57
  JUMPI 30

  RETURN 12
  CALL 3

  BLOCKHASH 1
  DIV 11
  MOD 3
