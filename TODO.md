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

## EtherPot Instruction Counts

  PUSH1 372
  MLOAD 32
  PUSH2 104
  RETURN 12
  PUSH4 13
  BLOCKHASH 1
  SHA3 31
  PUSH8 5
  STOP 2
  NUMBER 3
  CALLDATASIZE 1
  CALLVALUE 4
  DUP11 4
  DUP10 2
  EXP 6
  ISZERO 17
  MUL 9
  DIV 11
  SUB 17
  CALLDATALOAD 17
  PUSH20 10
  CALLER 4
  PUSH29 1
  POP 207
  DUP8 2
  JUMPI 30
  LT 5
  DUP5 6
  SSTORE 8
  JUMPDEST 82
  GT 7
  SWAP6 1
  MSTORE 77
  SWAP7 1
  DUP7 16
  JUMP 57
  ADD 130
  SWAP5 1
  SWAP2 44
  EQ 15
  AND 14
  DUP4 21
  SWAP4 5
  DUP6 7
  DUP1 75
  SWAP3 16
  DUP3 38
  SWAP1 121
  DUP2 104
  DUP9 5
  SLOAD 25
  CALL 3
  NOT 2
  OR 2
  MOD 3

