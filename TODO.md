# TODO


# BACKBURNER

  1. Figure out right semantics... I don't like the types yet
    * In particular, i'd like semantics to be similar to the small-step error
      semantics in DVH PL notes. Flat structure makes this difficult. -- 
  2. Convert to monadic interpreter?

# DONE

  1. Refactor to use lenses? -- DONE
  2. Rolling my own Set data structure was dumb, should just use Data.Set for now. Will probably need
     to change to provide heuristics (e.g. priority queue rather than set) but we can do that later. -- DONE
  3. Convert to use H halting function from yellow paper
  4. Collect final states as well as errors

## EtherPot Instruction Counts

  MLOAD 32
  RETURN 12
  BLOCKHASH 1
  SHA3 31
  CALLDATASIZE 1
  CALLVALUE 4
  CALLDATALOAD 17
  CALLER 4
  JUMPI 30
  SSTORE 8
  MSTORE 77
  JUMP 57
  SLOAD 25
  CALL 3

  EXP 6
  ISZERO 17
  MUL 9
  DIV 11
  SUB 17
  LT 5
  GT 7
  ADD 130
  EQ 15
  AND 14
  NOT 2
  OR 2
  MOD 3

