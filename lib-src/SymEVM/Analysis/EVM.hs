module SymEVM.Analysis.EVM where

import Data.Vector
import Data.Either
import Data.ByteString.Lazy
import Data.Word
import Data.Binary
import Data.LargeWord
import Control.Lens

import SymEVM.Data
import SymEVM.Analysis.Util as U

import qualified SymEVM.Data.Util.Set as S

--------------- type aliases --------------------------

type Work = (S.Set Err, S.Set State)

--------------- injection -----------------------------

baseState :: State
baseState 
  = State { world    = World    ()
          , _machine = Machine  { _pc = 0, _stack = [] }
          , substate = Substate ()
          , _env     = Env      { _code = error "Code is uninitialized!" }
          }

injectState :: Code -> State
injectState c = baseState & (env . code) .~ c

baseWork :: Work
baseWork = (S.empty, error "Worklist is uninitialized!")

injectWork :: State -> Work
injectWork st = baseWork & _2 .~ (S.singleton st)

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
  
--------------- `halt`, `err`, `instr`, and `step` relations -----------
halt :: State -> Bool
halt st = 
  let control = (st ^. env . code) ! (st ^. machine . pc) in
  case control of
    0x00 -> True -- STOP
    0xf3 -> True -- RETURN
    0xff -> True -- SUICIDE
    _    -> False

err :: State -> Either Err State
err st = Right st

-- | Produces the set of all next possible states. For concrete states, result will always be a set of size 1 which contains
--   the next state. For symbolic states, there could be many possible next states (e.g. multiple jump destinations).
instr :: State -> S.Set State
instr st =
  case control of
    0x00 -> -- STOP
      let st' = incrPC st in
      S.singleton st'
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
    opcode | 0x60 <= opcode && opcode <= 0x7f -> -- PUSH (TODO)
      let currPC   = st ^. machine . pc
          currCode = st ^. env . code

          n        = fromIntegral (opcode - 0x60 + 1)
          toPush   = decode . pad256 . pack . toList $ slice (currPC + 1) n currCode :: Word256

          st'      = addPC st  (n + 1)
          st''     = push  st' (CB256 toPush)
      in
      S.singleton st''
    opcode | 0x80 <= opcode && opcode <= 0x8f -> -- DUP (TODO)
      let currStack = st ^. machine . stack

          n         = fromIntegral (opcode - 0x80)
          
          st'       = incrPC st
          st''      = push st' (currStack !! n)
      in
      S.singleton st''
    opcode | 0x90 <= opcode && opcode <= 0x9f -> -- SWAP (TODO)
      let st' = incrPC st in
      S.singleton st'
    opcode | 0xa0 <= opcode && opcode <= 0xa4 -> -- LOG (TODO)
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

step' :: State -> S.Set (Either Err State) -- TODO: use H from yellow paper to determine halt (right now cheating by returning
step' st = S.map err (instr st)            --       empty list in `instr`

step :: State -> (S.Set Err, S.Set State)
step st = 
  let (l, r) = U.partitionEithers (step' st) in
  if halt st then 
    (l, S.empty)
  else
    (l, r)

--------------- driver -----------------------

oneWork :: Work -> Work
oneWork w =
  let (errs, sts)   = w
      (errs', sts') = U.flattenPairs (S.map step sts)
  in
  (S.union errs errs', sts')

doWork :: Work -> Work
doWork w =
  let (_, sts) = w in
  if S.null sts then
    w
  else
    doWork (oneWork w)

eval :: State -> S.Set Err
eval = fst . doWork . injectWork

check :: Code -> S.Set Err
check = eval . injectState

temp :: Code -> S.Set State
temp = snd . oneWork . injectWork . injectState
