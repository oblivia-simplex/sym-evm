module SymEVM.Analysis.EVM where

import qualified Data.Vector as V
import Data.Either
import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Binary
import Data.LargeWord
import Control.Lens
import Control.Monad.Gen

import SymEVM.Data
import SymEVM.Analysis.Util as U

import qualified SymEVM.Data.Util.Set as S

--------------- type aliases --------------------------

type Result = (S.Set Err, S.Set State)

--------------- injection -----------------------------

baseState :: State
baseState 
  = State 
      { world    = World    ()
      , _machine = Machine  { _pc = 0, _stack = [] }
      , substate = Substate ()
      , _env     = Env      
                     { _block = Block { _number = SB256 "number" }
                     , _code  = error "Code is uninitialized!" }
      , _cond    = STrue
      }

injectState :: Code -> State
injectState c = baseState & (env . code) .~ c

baseWork :: (S.Set State, Result)
baseWork = (error "Worklist is uninitialized!", (S.empty, S.empty))

injectWork :: State -> (S.Set State, Result)
injectWork st = baseWork & _1 .~ (S.singleton st)

--------------- relation helpers --------------

incrPC :: State -> State
incrPC st = addPC st 1

addPC :: State -> Int -> State
addPC st amt = st & (machine . pc) +~ amt

push :: State -> Symbol -> State
push st frame = st & (machine . stack) %~ ((:) frame)

pop :: State -> (Symbol, State)
pop st = 
  let (s0 : s') = st ^. machine . stack in
  (s0, st & (machine . stack) .~ s')

updateCond :: State -> Symbol -> State
updateCond st c = st & cond %~ (\cond -> SAnd c cond)

fresh :: Gen Integer Symbol
fresh =
  do
    x <- gen
    return $ SB256 ("x" ++ (show x))
  
--------------- `err`, `instr`, and `step` relations -----------

err :: State -> S.Set Err
err st = S.empty

-- | Produces the set of all next possible states. For concrete states, result will always be a set of size 1 which contains
--   the next state. For symbolic states, there could be many possible next states (e.g. multiple jump destinations).
instr :: State -> Gen Integer (S.Set State)
instr st =
  case control of
    0x00 -> -- STOP
      let st' = incrPC st in
      return $ S.singleton st'
    0x01 -> -- ADD (TODO)
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SAdd s0 s1))
        return $ S.singleton st_final
    0x02 -> -- MUL (TODO)
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SMult s0 s1))
        return $ S.singleton st_final
    0x03 -> -- SUB (TODO
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SSub s0 s1))
        return $ S.singleton st_final
    0x04 -> -- DIV (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x05 -> -- SDIV (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x06 -> -- MOD (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x07 -> -- SMOD (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x08 -> -- ADDMOD (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x09 -> -- MULMOD (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x0a -> -- EXP (TODO)
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SExp s0 s1))
        return $ S.singleton st_final
    0x0b -> -- SIGNEXTEND (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x10 -> -- LT (TODO)
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SLT s0 s1))
        return $ S.singleton st_final
    0x11 -> -- GT (TODO)
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SGT s0 s1))
        return $ S.singleton st_final
    0x12 -> -- SLT (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x13 -> -- SGT (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x14 -> -- EQ (TODO)
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SEQ s0 s1))
        return $ S.singleton st_final
    0x15 -> -- ISZERO (TODO)
      do
        let st_incr   = incrPC st
        let (s0, st') = pop st_incr
        fresh_sym    <- fresh
        let st_fresh  = push st' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SIsZero s0))
        return $ S.singleton st_final
    0x16 -> -- AND (TODO)
      do 
        let st_incr    = incrPC st
        let (s0, st')  = pop st_incr
        let (s1, st'') = pop st'
        fresh_sym     <- fresh
        let st_fresh  = push st'' fresh_sym
        let st_final  = updateCond st_fresh (SEq fresh_sym (SAND s0 s1))
        return $ S.singleton st_final
    0x17 -> -- OR (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x18 -> -- XOR (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x19 -> -- NOT (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x1a -> -- BYTE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x20 -> -- SHA3 (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x30 -> -- ADDRESS (TODO
      let st' = incrPC st in
      return $ S.singleton st'
    0x31 -> -- BALANCE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x32 -> -- ORIGIN (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x33 -> -- CALLER (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x34 -> -- CALLVALUE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x35 -> -- CALLDATALOAD (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x36 -> -- CALLDATASIZE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x37 -> -- CALLDATACOPY (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x38 -> -- CODESIZE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x39 -> -- CODECOPY (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x3a -> -- GASPRICE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x3b -> -- EXTCODESIZE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x3c -> -- EXTCODECOPY (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x40 -> -- BLOCKHASH (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x41 -> -- COINBASE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x42 -> -- TIMESTAMP (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x43 -> -- NUMBER
      let st'  = incrPC st
          st'' = push st' (st' ^. env . block . number)
      in
      return $ S.singleton st''
    0x44 -> -- DIFFICULTY (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x45 -> -- GASLIMIT (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x50 -> -- POP
      let st'       = incrPC st
          (_, st'') = pop st'
      in
      return $ S.singleton st''
    0x51 -> -- MLOAD (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x52 -> -- MSTORE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x53 -> -- MSTORE8 (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x54 -> -- SLOAD (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x55 -> -- SSTORE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x56 -> -- JUMP (TODO)
      return $ S.empty
    0x57 -> -- JUMPI (TODO)
      return $ S.empty
    0x58 -> -- PC (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x59 -> -- MSIZE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x5a -> -- GAS (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0x5b -> -- JUMPDEST
      let st' = incrPC st in
      return $ S.singleton st'
    opcode | 0x60 <= opcode && opcode <= 0x7f -> -- PUSH (TODO)
      let currPC   = st ^. machine . pc
          currCode = st ^. env . code

          n        = fromIntegral (opcode - 0x60 + 1)
          toPush   = decode . pad256 . B.pack . V.toList $ V.slice (currPC + 1) n currCode :: Word256

          st'      = addPC st  (n + 1)
          st''     = push  st' (CB256 toPush)
      in
      return $ S.singleton st''
    opcode | 0x80 <= opcode && opcode <= 0x8f -> -- DUP (TODO)
      let currStack = st ^. machine . stack

          n         = fromIntegral (opcode - 0x80 + 1)
          
          st'       = incrPC st
          st''      = push st' (currStack !! (n - 1))
      in
      return $ S.singleton st''
    opcode | 0x90 <= opcode && opcode <= 0x9f -> -- SWAP (TODO)
      let currStack = st ^. machine . stack

          n         = fromIntegral (opcode - 0x90 + 1)
          (l, r)    = splitAt n currStack
          (s0 : l') = l
          (sn : r') = r
          stack'    = sn : (l' ++ (s0 : r'))

          st'       = incrPC st
          st''      = st' & (machine . stack) .~ stack'
      in
      return $ S.singleton st''
    opcode | 0xa0 <= opcode && opcode <= 0xa4 -> -- LOG (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0xf0 -> -- CREATE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0xf1 -> -- CALL (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0xf2 -> -- CALLCODE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0xf3 -> -- RETURN (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0xf4 -> -- DELEGATECALL (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
    0xff -> -- SUICIDE (TODO)
      let st' = incrPC st in
      return $ S.singleton st'
  where
    control = (st ^. env . code) V.! (st ^. machine . pc)

step :: State -> Gen Integer (S.Set Err, S.Set State)
step st = 
  do
    instr_st <- instr st
    return (err st, instr_st)

--------------- driver -----------------------

halt :: State -> Bool
halt st = 
  let control = (st ^. env . code) V.! (st ^. machine . pc) in
  case control of
    0x00 -> True -- STOP
    0xf3 -> True -- RETURN
    0xff -> True -- SUICIDE
    _    -> False

oneWork :: (S.Set State, Result) -> Gen Integer (S.Set State, Result)
oneWork curr =
  do
    let (work, r)           = curr
    let (errs, finals)      = r
    
    let (toStep, work_rem)  = S.deleteFindMin work
    (errs', tmp)           <- step toStep
    let (finals', work_new) = S.partition halt tmp

    return (S.union work_rem work_new, (S.union errs errs', S.union finals finals'))

doWork :: (S.Set State, Result) -> Gen Integer Result
doWork curr =
  let (work, r) = curr in
  if S.null work then
    return r
  else
    oneWork curr >>= doWork

eval :: Code -> Result
eval = runGen . doWork . injectWork . injectState

run :: Code -> S.Set State
run = snd . eval

check :: Code -> S.Set Err
check = fst . eval
