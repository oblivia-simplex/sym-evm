module SymEVM.Data.Util.Instr
  ( Instr(..)
  , InstrMeta(..)
  , desc
  , delta
  , alpha
  , instrMeta
  ) where

import Control.Lens

import Data.Word
import Data.Map

import Prelude hiding (LT, GT, EQ)

data Instr
  = STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | SDIV
  | MOD
  | SMOD
  | ADDMOD
  | MULMOD
  | EXP
  | SIGNEXTEND
  | LT
  | GT
  | SLT
  | SGT
  | EQ
  | ISZERO
  | AND
  | OR
  | XOR
  | NOT
  | BYTE
  | SHA3
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATALOAD
  | CALLDATASIZE
  | CALLDATACOPY
  | CODESIZE
  | CODECOPY
  | GASPRICE
  | EXTCODESIZE
  | EXTCODECOPY
  | BLOCKHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | PC
  | MSIZE
  | GAS
  | JUMPDEST
  | PUSHN Int
  | DUPN Int
  | SWAPN Int
  | LOGN Int
  | CREATE
  | CALL
  | CALLCODE
  | RETURN
  | DELEGATECALL
  | SUICIDE deriving ( Show, Eq, Ord )

data InstrMeta = InstrMeta
  { _desc  :: Instr
  , _delta :: Int
  , _alpha :: Int
  } deriving ( Show, Eq, Ord )

desc :: Lens' InstrMeta Instr
desc = lens _desc (\instrmeta newDesc -> instrmeta { _desc = newDesc })

delta :: Lens' InstrMeta Int
delta = lens _delta (\instrmeta newDelta -> instrmeta { _delta = newDelta })

alpha :: Lens' InstrMeta Int
alpha = lens _alpha (\instrmeta newAlpha -> instrmeta { _alpha = newAlpha })

instrMeta :: Map Word8 InstrMeta
instrMeta = fromList
  [ (0x00, InstrMeta { _desc  = STOP        , _delta = 0 , _alpha = 0  })
  , (0x01, InstrMeta { _desc  = ADD         , _delta = 2 , _alpha = 1  })
  , (0x02, InstrMeta { _desc  = MUL         , _delta = 2 , _alpha = 1  })
  , (0x03, InstrMeta { _desc  = SUB         , _delta = 2 , _alpha = 1  })
  , (0x04, InstrMeta { _desc  = DIV         , _delta = 2 , _alpha = 1  })
  , (0x05, InstrMeta { _desc  = SDIV        , _delta = 2 , _alpha = 1  })
  , (0x06, InstrMeta { _desc  = MOD         , _delta = 2 , _alpha = 1  })
  , (0x07, InstrMeta { _desc  = SMOD        , _delta = 2 , _alpha = 1  })
  , (0x08, InstrMeta { _desc  = ADDMOD      , _delta = 3 , _alpha = 1  })
  , (0x09, InstrMeta { _desc  = MULMOD      , _delta = 3 , _alpha = 1  })
  , (0x0a, InstrMeta { _desc  = EXP         , _delta = 2 , _alpha = 1  })
  , (0x0b, InstrMeta { _desc  = SIGNEXTEND  , _delta = 2 , _alpha = 1  })
  , (0x10, InstrMeta { _desc  = LT          , _delta = 2 , _alpha = 1  })
  , (0x11, InstrMeta { _desc  = GT          , _delta = 2 , _alpha = 1  })
  , (0x12, InstrMeta { _desc  = SLT         , _delta = 2 , _alpha = 1  })
  , (0x13, InstrMeta { _desc  = SLT         , _delta = 2 , _alpha = 1  })
  , (0x14, InstrMeta { _desc  = EQ          , _delta = 2 , _alpha = 1  })
  , (0x15, InstrMeta { _desc  = ISZERO      , _delta = 1 , _alpha = 1  })
  , (0x16, InstrMeta { _desc  = AND         , _delta = 2 , _alpha = 1  })
  , (0x17, InstrMeta { _desc  = OR          , _delta = 2 , _alpha = 1  })
  , (0x18, InstrMeta { _desc  = XOR         , _delta = 2 , _alpha = 1  })
  , (0x19, InstrMeta { _desc  = NOT         , _delta = 1 , _alpha = 1  })
  , (0x1a, InstrMeta { _desc  = BYTE        , _delta = 2 , _alpha = 1  })
  , (0x20, InstrMeta { _desc  = SHA3        , _delta = 2 , _alpha = 1  })
  , (0x30, InstrMeta { _desc  = ADDRESS     , _delta = 0 , _alpha = 1  })
  , (0x31, InstrMeta { _desc  = BALANCE     , _delta = 1 , _alpha = 1  })
  , (0x32, InstrMeta { _desc  = ORIGIN      , _delta = 0 , _alpha = 1  })
  , (0x33, InstrMeta { _desc  = CALLER      , _delta = 0 , _alpha = 1  })
  , (0x34, InstrMeta { _desc  = CALLVALUE   , _delta = 0 , _alpha = 1  })
  , (0x35, InstrMeta { _desc  = CALLDATALOAD, _delta = 1 , _alpha = 1  })
  , (0x36, InstrMeta { _desc  = CALLDATASIZE, _delta = 0 , _alpha = 1  })
  , (0x37, InstrMeta { _desc  = CALLDATACOPY, _delta = 3 , _alpha = 0  })
  , (0x38, InstrMeta { _desc  = CODESIZE    , _delta = 0 , _alpha = 1  })
  , (0x39, InstrMeta { _desc  = CODECOPY    , _delta = 3 , _alpha = 0  })
  , (0x3a, InstrMeta { _desc  = GASPRICE    , _delta = 0 , _alpha = 1  })
  , (0x3b, InstrMeta { _desc  = EXTCODESIZE , _delta = 1 , _alpha = 1  })
  , (0x3c, InstrMeta { _desc  = EXTCODECOPY , _delta = 4 , _alpha = 0  })
  , (0x40, InstrMeta { _desc  = BLOCKHASH   , _delta = 1 , _alpha = 1  })
  , (0x41, InstrMeta { _desc  = COINBASE    , _delta = 0 , _alpha = 1  })
  , (0x42, InstrMeta { _desc  = TIMESTAMP   , _delta = 0 , _alpha = 1  })
  , (0x43, InstrMeta { _desc  = NUMBER      , _delta = 0 , _alpha = 1  })
  , (0x44, InstrMeta { _desc  = DIFFICULTY  , _delta = 0 , _alpha = 1  })
  , (0x45, InstrMeta { _desc  = GASLIMIT    , _delta = 0 , _alpha = 1  })
  , (0x50, InstrMeta { _desc  = POP         , _delta = 1 , _alpha = 0  })
  , (0x51, InstrMeta { _desc  = MLOAD       , _delta = 1 , _alpha = 1  })
  , (0x52, InstrMeta { _desc  = MSTORE      , _delta = 2 , _alpha = 0  })
  , (0x53, InstrMeta { _desc  = MSTORE8     , _delta = 2 , _alpha = 0  })
  , (0x54, InstrMeta { _desc  = SLOAD       , _delta = 1 , _alpha = 1  })
  , (0x55, InstrMeta { _desc  = SSTORE      , _delta = 2 , _alpha = 0  })
  , (0x56, InstrMeta { _desc  = JUMP        , _delta = 1 , _alpha = 0  })
  , (0x57, InstrMeta { _desc  = JUMPI       , _delta = 2 , _alpha = 0  })
  , (0x58, InstrMeta { _desc  = PC          , _delta = 0 , _alpha = 1  })
  , (0x59, InstrMeta { _desc  = MSIZE       , _delta = 0 , _alpha = 1  })
  , (0x5a, InstrMeta { _desc  = GAS         , _delta = 0 , _alpha = 1  })
  , (0x5b, InstrMeta { _desc  = JUMPDEST    , _delta = 0 , _alpha = 0  })
  , (0x60, InstrMeta { _desc  = PUSHN 1     , _delta = 0 , _alpha = 1  })
  , (0x61, InstrMeta { _desc  = PUSHN 2     , _delta = 0 , _alpha = 1  })
  , (0x62, InstrMeta { _desc  = PUSHN 3     , _delta = 0 , _alpha = 1  })
  , (0x63, InstrMeta { _desc  = PUSHN 4     , _delta = 0 , _alpha = 1  })
  , (0x64, InstrMeta { _desc  = PUSHN 5     , _delta = 0 , _alpha = 1  })
  , (0x65, InstrMeta { _desc  = PUSHN 6     , _delta = 0 , _alpha = 1  })
  , (0x66, InstrMeta { _desc  = PUSHN 7     , _delta = 0 , _alpha = 1  })
  , (0x67, InstrMeta { _desc  = PUSHN 8     , _delta = 0 , _alpha = 1  })
  , (0x68, InstrMeta { _desc  = PUSHN 9     , _delta = 0 , _alpha = 1  })
  , (0x69, InstrMeta { _desc  = PUSHN 10    , _delta = 0 , _alpha = 1  })
  , (0x6a, InstrMeta { _desc  = PUSHN 11    , _delta = 0 , _alpha = 1  })
  , (0x6b, InstrMeta { _desc  = PUSHN 12    , _delta = 0 , _alpha = 1  })
  , (0x6c, InstrMeta { _desc  = PUSHN 13    , _delta = 0 , _alpha = 1  })
  , (0x6d, InstrMeta { _desc  = PUSHN 14    , _delta = 0 , _alpha = 1  })
  , (0x6e, InstrMeta { _desc  = PUSHN 15    , _delta = 0 , _alpha = 1  })
  , (0x6f, InstrMeta { _desc  = PUSHN 16    , _delta = 0 , _alpha = 1  })
  , (0x70, InstrMeta { _desc  = PUSHN 17    , _delta = 0 , _alpha = 1  })
  , (0x71, InstrMeta { _desc  = PUSHN 18    , _delta = 0 , _alpha = 1  })
  , (0x72, InstrMeta { _desc  = PUSHN 19    , _delta = 0 , _alpha = 1  })
  , (0x73, InstrMeta { _desc  = PUSHN 20    , _delta = 0 , _alpha = 1  })
  , (0x74, InstrMeta { _desc  = PUSHN 21    , _delta = 0 , _alpha = 1  })
  , (0x75, InstrMeta { _desc  = PUSHN 22    , _delta = 0 , _alpha = 1  })
  , (0x76, InstrMeta { _desc  = PUSHN 23    , _delta = 0 , _alpha = 1  })
  , (0x77, InstrMeta { _desc  = PUSHN 24    , _delta = 0 , _alpha = 1  })
  , (0x78, InstrMeta { _desc  = PUSHN 25    , _delta = 0 , _alpha = 1  })
  , (0x79, InstrMeta { _desc  = PUSHN 26    , _delta = 0 , _alpha = 1  })
  , (0x7a, InstrMeta { _desc  = PUSHN 27    , _delta = 0 , _alpha = 1  })
  , (0x7b, InstrMeta { _desc  = PUSHN 28    , _delta = 0 , _alpha = 1  })
  , (0x7c, InstrMeta { _desc  = PUSHN 29    , _delta = 0 , _alpha = 1  })
  , (0x7d, InstrMeta { _desc  = PUSHN 30    , _delta = 0 , _alpha = 1  })
  , (0x7e, InstrMeta { _desc  = PUSHN 31    , _delta = 0 , _alpha = 1  })
  , (0x7f, InstrMeta { _desc  = PUSHN 32    , _delta = 0 , _alpha = 1  })
  , (0x80, InstrMeta { _desc  = DUPN 1      , _delta = 1 , _alpha = 2  })
  , (0x81, InstrMeta { _desc  = DUPN 2      , _delta = 2 , _alpha = 3  })
  , (0x82, InstrMeta { _desc  = DUPN 3      , _delta = 3 , _alpha = 4  })
  , (0x83, InstrMeta { _desc  = DUPN 4      , _delta = 4 , _alpha = 5  })
  , (0x84, InstrMeta { _desc  = DUPN 5      , _delta = 5 , _alpha = 6  })
  , (0x85, InstrMeta { _desc  = DUPN 6      , _delta = 6 , _alpha = 7  })
  , (0x86, InstrMeta { _desc  = DUPN 7      , _delta = 7 , _alpha = 8  })
  , (0x87, InstrMeta { _desc  = DUPN 8      , _delta = 8 , _alpha = 9  })
  , (0x88, InstrMeta { _desc  = DUPN 9      , _delta = 9 , _alpha = 10 })
  , (0x89, InstrMeta { _desc  = DUPN 10     , _delta = 10, _alpha = 11 })
  , (0x8a, InstrMeta { _desc  = DUPN 11     , _delta = 11, _alpha = 12 })
  , (0x8b, InstrMeta { _desc  = DUPN 12     , _delta = 12, _alpha = 13 })
  , (0x8c, InstrMeta { _desc  = DUPN 13     , _delta = 13, _alpha = 14 })
  , (0x8d, InstrMeta { _desc  = DUPN 14     , _delta = 14, _alpha = 15 })
  , (0x8e, InstrMeta { _desc  = DUPN 15     , _delta = 15, _alpha = 16 })
  , (0x8f, InstrMeta { _desc  = DUPN 16     , _delta = 16, _alpha = 17 })
  , (0x90, InstrMeta { _desc  = SWAPN 1     , _delta = 2 , _alpha = 2  })
  , (0x91, InstrMeta { _desc  = SWAPN 2     , _delta = 3 , _alpha = 3  })
  , (0x92, InstrMeta { _desc  = SWAPN 3     , _delta = 4 , _alpha = 4  })
  , (0x93, InstrMeta { _desc  = SWAPN 4     , _delta = 5 , _alpha = 5  })
  , (0x94, InstrMeta { _desc  = SWAPN 5     , _delta = 6 , _alpha = 6  })
  , (0x95, InstrMeta { _desc  = SWAPN 6     , _delta = 7 , _alpha = 7  })
  , (0x96, InstrMeta { _desc  = SWAPN 7     , _delta = 8 , _alpha = 8  })
  , (0x97, InstrMeta { _desc  = SWAPN 8     , _delta = 9 , _alpha = 9  })
  , (0x98, InstrMeta { _desc  = SWAPN 9     , _delta = 10, _alpha = 10 })
  , (0x99, InstrMeta { _desc  = SWAPN 10    , _delta = 11, _alpha = 11 })
  , (0x9a, InstrMeta { _desc  = SWAPN 11    , _delta = 12, _alpha = 12 })
  , (0x9b, InstrMeta { _desc  = SWAPN 12    , _delta = 13, _alpha = 13 })
  , (0x9c, InstrMeta { _desc  = SWAPN 13    , _delta = 14, _alpha = 14 })
  , (0x9d, InstrMeta { _desc  = SWAPN 14    , _delta = 15, _alpha = 15 })
  , (0x9e, InstrMeta { _desc  = SWAPN 15    , _delta = 16, _alpha = 16 })
  , (0x9f, InstrMeta { _desc  = SWAPN 16    , _delta = 17, _alpha = 17 })
  , (0xa0, InstrMeta { _desc  = LOGN 0      , _delta = 2 , _alpha = 0  })
  , (0xa1, InstrMeta { _desc  = LOGN 1      , _delta = 3 , _alpha = 0  })
  , (0xa2, InstrMeta { _desc  = LOGN 2      , _delta = 4 , _alpha = 0  })
  , (0xa3, InstrMeta { _desc  = LOGN 3      , _delta = 5 , _alpha = 0  })
  , (0xa4, InstrMeta { _desc  = LOGN 4      , _delta = 6 , _alpha = 0  })
  , (0xf0, InstrMeta { _desc  = CREATE      , _delta = 3 , _alpha = 1  })
  , (0xf1, InstrMeta { _desc  = CALL        , _delta = 7 , _alpha = 1  })
  , (0xf2, InstrMeta { _desc  = CALLCODE    , _delta = 7 , _alpha = 1  })
  , (0xf3, InstrMeta { _desc  = RETURN      , _delta = 2 , _alpha = 0  })
  , (0xf4, InstrMeta { _desc  = DELEGATECALL, _delta = 6 , _alpha = 1  })
  , (0xff, InstrMeta { _desc  = SUICIDE     , _delta = 1 , _alpha = 0  })
  ]
