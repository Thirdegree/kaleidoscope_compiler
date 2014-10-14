module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

type SymbolTable = [(String, Operand)]

data CodegenState
	= CodegenState {
	currentBlock 	:: Name,
	blocks 			:: Map.Map Name BlockState,
	symtab 			:: SymbolTable,
	blockCount 		:: Int,
	count 			:: Word,
	names 			:: Names
	} deriving Show

data BlockState
	= BlockState {
	idx 	:: Int,
	stack 	:: [Named Instruction],
	term 	:: Maybe (Named Terminator)
	} deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a}
	deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM { unLLVM :: State AST.Module a}
	deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

cc :: (Monad a) => a -> a
cc' :: Monad a => a -> a

addDefn :: Definition -> LLVM()
addDefn d = do
	defs <- gets moduleDefinitions
	modify $ \s -> s { moduleDefinitions = defs ++ [d]}

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys = addDefn $
	GlobalDefinitions $ functionDefaults {
	name = Name label,
	parameters = ([parameters ty nm [] | (ty, nm)<-argtys], False),
	returnType = retty,
	basicBlocks = []
	}

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
	bls <- gets blocks
	ix <- gets blockCount
	nms <- gets names

	let new = emptyBlock ix
		(qname, supply) = uniqueName bname nms

	modify $ \s -> s { blocks = Map.insert (Name qname) new bls,
					   blockCount = ix + 1,
					   names = supply
					 }

	return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
	modify $ \s -> s { currentBlock = bname }
	return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
	active <- gets currentBlock
	modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
	c <- gets currentBlock
	blks <- gets blocks
	case Map.lookup c blks of
		Just x -> return x
		Nothing -> error $ "No such block: " ++ show c




