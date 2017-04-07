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

## EtherPot Instruction Counts

  PUSH1 372 -- X
  MLOAD 32
  PUSH2 104 -- X
  RETURN 12
  PUSH4 13  -- X
  BLOCKHASH 1
  SHA3 31
  PUSH8 5 -- X
  STOP 2
  NUMBER 3
  CALLDATASIZE 1
  CALLVALUE 4
  DUP11 4 -- X
  DUP10 2 -- X
  EXP 6
  ISZERO 17
  MUL 9
  DIV 11
  SUB 17
  CALLDATALOAD 17
  PUSH20 10 -- X
  CALLER 4
  PUSH29 1 -- X
  POP 207
  DUP8 2 -- X
  JUMPI 30
  LT 5
  DUP5 6 -- X
  SSTORE 8
  JUMPDEST 82
  GT 7
  SWAP6 1
  MSTORE 77
  SWAP7 1
  DUP7 16 -- X
  JUMP 57
  ADD 130
  SWAP5 1
  SWAP2 44
  EQ 15
  AND 14
  DUP4 21 -- X
  SWAP4 5
  DUP6 7 -- X
  DUP1 75 -- X
  SWAP3 16
  DUP3 38 -- X
  SWAP1 121
  DUP2 104 -- X
  DUP9 5 -- X
  SLOAD 25
  CALL 3
  NOT 2
  OR 2
  MOD 3

