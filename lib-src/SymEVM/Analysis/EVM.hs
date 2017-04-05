module SymEVM.Analysis.EVM where

import Data.Array
import Data.Either
import Control.Lens

import SymEVM.Data

import qualified SymEVM.Data.Util.Set as S

baseState :: State
baseState 
  = State { world    = World    ()
          , _machine = Machine  { _pc = 0, _stack = [] }
          , substate = Substate ()
          , _env     = Env      { _code = error "Code is uninitialized!" }
          }

--------------- `instr` and `err` helpers --------------

incrPC :: State -> State
incrPC st = addPC st 1

addPC :: State -> Integer -> State
addPC st amt = st & (machine . pc) +~ amt

pop :: State -> (Symbol, State)
pop st = 
  let (s0 : s') = st ^. machine . stack in
  (s0, st & (machine . stack) .~ s')
  
--------------- `instr` and `err` relations -----------

err :: State -> Either Err State
err st = Right st

-- | Produces the set of all next possible states. For concrete states, result will always be a set of size 1 which contains
--   the next state. For symbolic states, there could be many possible next states (e.g. multiple jump destinations).
instr :: State -> S.Set State
instr st =
  case control of
    0x00 -> -- STOP
      S.empty
    0x01 -> -- ADD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x02 -> -- MUL (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x03 -> -- SUB (TODO
      let st' = incrPC st in
      S.singleton st'
    0x04 -> -- DIV (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x05 -> -- SDIV (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x06 -> -- MOD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x07 -> -- SMOD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x08 -> -- ADDMOD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x09 -> -- MULMOD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x0a -> -- EXP (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x0b -> -- SIGNEXTEND (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x10 -> -- LT (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x11 -> -- GT (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x12 -> -- SLT (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x13 -> -- SGT (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x14 -> -- EQ (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x15 -> -- ISZERO (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x16 -> -- AND (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x17 -> -- OR (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x18 -> -- XOR (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x19 -> -- NOT (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x1a -> -- BYTE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x20 -> -- SHA3 (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x30 -> -- ADDRESS (TODO
      let st' = incrPC st in
      S.singleton st'
    0x31 -> -- BALANCE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x32 -> -- ORIGIN (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x33 -> -- CALLER (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x34 -> -- CALLVALUE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x35 -> -- CALLDATALOAD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x36 -> -- CALLDATASIZE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x37 -> -- CALLDATACOPY (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x38 -> -- CODESIZE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x39 -> -- CODECOPY (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x3a -> -- GASPRICE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x3b -> -- EXTCODESIZE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x3c -> -- EXTCODECOPY (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x40 -> -- BLOCKHASH (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x41 -> -- COINBASE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x42 -> -- TIMESTAMP (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x43 -> -- NUMBER (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x44 -> -- DIFFICULTY (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x45 -> -- GASLIMIT (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x50 -> -- POP (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x51 -> -- MLOAD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x52 -> -- MSTORE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x53 -> -- MSTORE8 (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x54 -> -- SLOAD (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x55 -> -- SSTORE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x56 -> -- JUMP (TODO)
      S.empty
    0x57 -> -- JUMPI (TODO)
      S.empty
    0x58 -> -- PC (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x59 -> -- MSIZE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x5a -> -- GAS (TODO)
      let st' = incrPC st in
      S.singleton st'
    0x5b -> -- JUMPDEST
      let st' = incrPC st in
      S.singleton st'
    opcode | 0x60 <= opcode && opcode <= 0x7f -> -- PUSH
      let st' = addPC st (toInteger (opcode - 0x60 + 2)) in
      S.singleton st'
    opcode | 0x80 <= opcode && opcode <= 0x8f -> -- DUP
      let st' = incrPC st in
      S.singleton st'
    opcode | 0x90 <= opcode && opcode <= 0x9f -> -- SWAP
      let st' = incrPC st in
      S.singleton st'
    opcode | 0xa0 <= opcode && opcode <= 0xa4 -> -- LOG
      let st' = incrPC st in
      S.singleton st'
    0xf0 -> -- CREATE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0xf1 -> -- CALL (TODO)
      let st' = incrPC st in
      S.singleton st'
    0xf2 -> -- CALLCODE (TODO)
      let st' = incrPC st in
      S.singleton st'
    0xf3 -> -- RETURN (TODO)
      let st' = incrPC st in
      S.singleton st'
    0xf4 -> -- DELEGATECALL (TODO)
      let st' = incrPC st in
      S.singleton st'
    0xff -> -- SUICIDE (TODO)
      let st' = incrPC st in
      S.singleton st'
  where
    control = (st ^. env . code) ! (st ^. machine . pc)

--------------- `step` relation -----------------------

stepBug :: State -> S.Set (Either Err State)
stepBug st = S.map err (instr st)

stepErr :: State -> (S.Set Err, S.Set State)
stepErr st = partitionEithers (stepBug st)
  where
    partitionEithers eithers = S.foldl partitionEither (S.empty, S.empty) eithers -- TODO: Put in Data.Either extension module
    partitionEither acc curr = case curr of
                                 Left  val -> (S.insert val (fst acc), snd acc)
                                 Right val -> (fst acc, S.insert val (snd acc))

eval' :: (S.Set Err, S.Set State) -> (S.Set Err, S.Set State)
eval' curr =
  let errs  = fst curr                           
      sts   = snd curr                           
      tmp   = flattenPairs (S.map stepErr sts)   
      errs' = fst tmp                            
      sts'  = snd tmp                          
  in
  (S.union errs errs', sts')
  where
    flattenPairs pairs   = S.foldl flattenPair (S.empty, S.empty) pairs -- TODO: Put in utility module
    flattenPair acc curr = (S.union (fst acc) (fst curr), S.union (snd acc) (snd curr))

iter :: (S.Set Err, S.Set State) -> (S.Set Err, S.Set State)
iter curr =
  let sts = snd curr in
  if S.null sts then
    curr
  else
    iter (eval' curr)

eval :: State -> S.Set Err
eval st = fst (iter (S.empty, S.singleton st))

inject :: Code -> State
inject c = baseState & (env . code) .~ c

check :: Code -> S.Set Err
check = eval . inject
