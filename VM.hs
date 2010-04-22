-- Virtual Machine/Interpreter for a dictionaryish language (think Javascript or Python)

module Main where

import qualified Data.Map as Map
import qualified Control.Monad.State as State
import Control.Monad
import Control.Monad.Trans
import Data.Data
import Data.Dynamic
import Data.IORef
import Data.Array

type Key = Object
type KeyString = String
type Val = Object
type Self = Object
type IdNumber = Integer
type Result = Object
type GetAttr = Self -> KeyString -> Action Result
type SetAttr = Self -> KeyString -> Val -> Action Result
type GetID = Self -> Action IdNumber
type IsEqual = Object -> Object -> Action Result
type Comparator = Object -> Object -> Action Ordering
type FnArgs = [Object]

type BinaryFn = Object -> Object -> Action Object

instance Show BinaryFn where
    show fn = "<BinaryFn>"

type UnaryFn = Object -> Action Object

instance Show UnaryFn where
    show fn = "<UnaryFn>"

type Call = (FnArgs -> Action Object)

instance Show Call where
    show call = "<Call>"

data Object = Object {
    oIdNumber :: IdNumber,
    oType :: Type,
    oValue :: Value
} deriving (Show)

type Dictionary = Map.Map Integer [Object]
type MutableDictionary = IORef Dictionary

instance Show MutableDictionary where
    show s = "<MutableDictionary>"

type List = [Object]
type MutableList = IORef List

instance Show MutableList where
    show s = "MutableList"

type LVal = String
type ExceptionObject = Object
type ExceptionType = Object

data Frame = Frame {
    fScope :: [Map.Map LVal Object],
    fCode :: Code,
    fCounter :: Int
} deriving (Show)

-- Program code is just an array of operations
data Code = Code {
    coOps :: Array Int Op,
    coConstants :: Map.Map Int Constant
} deriving (Show, Read)

data Constant = CInteger Integer
    | CDouble Double
    | CString String
    | CNone
    | CBool Bool
    deriving (Ord, Eq, Show, Read)

data Value = ValString String
    | ValInteger Integer
    | ValBool Bool
    | ValDouble Double
    | ValDict Dictionary
    | ValMutableDict MutableDictionary
    | ValList List
    | ValMutableList MutableList
    | ValFn Call
    | ValNone
    | ValDynamic Dynamic
    deriving (Show)

data Type = Type {
    tAdd :: BinaryFn, -- Add operator
    tMultiply :: BinaryFn, -- Multiply operator
    tDivide :: BinaryFn, -- Multiply operator
    tSubtract :: BinaryFn, -- Multiply operator
    tPower :: BinaryFn, -- Power operator
    tName :: String
} deriving (Show)

emptyType = Type {
    tAdd = nullBinaryFn,
    tMultiply = nullBinaryFn,
    tDivide = nullBinaryFn,
    tSubtract = nullBinaryFn,
    tPower = nullBinaryFn,
    tName = ""
}

noneType = emptyType {tName = "none"}

-- memo this somehow
newNoneObject = newObject ValNone noneType

nullBinaryFn left right = newNoneObject
nullUnaryFn obj = newNoneObject

data Op = Stop
    | Jump Int
	| Pop
	| PopTwo
	| RotThree
	| DupTop
	| RotFour
	| Nop
	| Binary BinOp
    | Unary UnOp
	| Print
    | LoadConst Int
    | Return
    deriving (Ord, Eq, Show, Read)

data UnOp = Positive | Negative | Not | Invert
    deriving (Ord, Read, Eq, Show)

data BinOp = Add | Subtract | Divide | Multiply | Power
    deriving (Ord, Read, Eq, Show)

data VM = VM {
    vmFrames :: [Frame],
    vmStack :: [Object],
    vmObjectCounter :: Counter
} deriving (Show)

type Counter = IORef Integer

instance Show Counter where
    show counter = "<Counter>"

intType = emptyType {
    tName = "int",
    tAdd = addInteger,
    tMultiply = multiplyInteger,
    tDivide = divideInteger,
    tPower = powerInteger
}

instance Eq Type where
    (==) left right = (tName left) == (tName right)

instance Ord Type where
    left < right = (tName left) < (tName right)

addInteger :: BinaryFn
addInteger (Object {oValue=(ValInteger i)}) (Object {oValue=(ValInteger j)}) = newIntegerObject (i + j)
addInteger left right = newNoneObject

multiplyInteger :: BinaryFn
multiplyInteger (Object {oValue=(ValInteger i)}) (Object {oValue=(ValInteger j)}) = newIntegerObject (i * j)
multiplyInteger left right = newNoneObject

divideInteger :: BinaryFn
divideInteger (Object {oValue=(ValInteger i)}) (Object {oValue=(ValInteger j)}) = newIntegerObject (i `div` j)
divideInteger left right = newNoneObject

powerInteger :: BinaryFn
powerInteger (Object {oValue=(ValInteger i)}) (Object {oValue=(ValInteger j)}) = newIntegerObject (i ^ j)
powerInteger left right = newNoneObject

subtractInteger :: BinaryFn
subtractInteger (Object {oValue=(ValInteger i)}) (Object {oValue=(ValInteger j)}) = newIntegerObject (i - j)
subtractInteger left right = newNoneObject

unexpectedType obj1 obj2 = error $ "bad type"

newCounter initial = newIORef initial
incCounter c = atomicModifyIORef c (\x -> (x + 1, x))

emptyCode = Code (array (0, 0) [(0, Return)]) Map.empty

emptyVM = do
    c <- newCounter 1000
    return $ VM [] [] c

pushFrame frame = State.modify $ \vm -> vm { vmFrames = frame : (vmFrames vm) }

popFrame :: Action ()
popFrame = State.modify $ \i@(VM{vmFrames=xs}) -> i{vmFrames=tail xs}

pushStack :: Object -> Action ()
pushStack o = State.modify $ \vm@(VM {vmStack=xs}) -> vm {vmStack=o:xs}

popStack :: Action Object
popStack = do
    vm <- State.get
    let item = head $ vmStack vm
    case vmStack vm of
        [] -> error $ "unexpected pop from empty stack"
        (x : xs) -> do
            State.put $ vm { vmStack = xs }
            return x

emptyFrame = Frame { fCode = undefined, fCounter = 0, fScope = [] }

nopFrame = emptyFrame {fCode = emptyCode}

-- Run some bytecodes from stdin
runProgram = do
   (code :: Code) <- liftIO $ liftM read getContents
   pushFrame $ emptyFrame {fCode = code }
   -- Run it!
   run
   -- Print the stack (should be 1 now)
   printStack

-- Print the current stack for debugging
printStack :: Action ()
printStack = do
    vm <- State.get
    liftIO $ print $ map oValue $ vmStack vm

newObject value valueType = do
    objectID <- nextObjectID
    return $ Object objectID valueType value

nextObjectID :: Action Integer
nextObjectID = do
    vm <- State.get
    liftIO $ incCounter $ vmObjectCounter vm

newIntegerObject :: Integer -> Action Object
newIntegerObject i = newObject (ValInteger i) intType

type Action a = State.StateT VM IO a

run :: Action ()
run = while step

while m = ifM m (while m) (return ())

ifM mBool left right = do
    t <- mBool
    if t then left else right

modifyTopFrame f = State.modify modifyTopFrame'
    where modifyTopFrame' vm@(VM {vmFrames = []})     = vm
          modifyTopFrame' vm@(VM {vmFrames = (x:xs)}) = vm {vmFrames = (f x) : xs}

incTopFrameCounter :: Action ()
incTopFrameCounter = modifyTopFrame $ \frame@(Frame {fCounter = i}) -> frame {fCounter = 1 + i}

topFrame :: Action (Maybe Frame)
topFrame = State.gets $ safeHead . vmFrames
    where
        safeHead [] = Nothing
        safeHead (x : xs) = Just x

withTopFrame :: (Frame -> Maybe b) -> Action (Maybe b)
withTopFrame f = do
    x <- liftM (fmap f) topFrame
    case x of
        (Just (Just x)) -> return $ Just x
        _ -> return Nothing

-- Fetch the next op
nextOp :: Action (Maybe Op)
nextOp = withTopFrame $ nextOp'
    where nextOp' (Frame {fCode=code, fCounter=counter}) =
            if between (bounds $ coOps code) counter
                    then Just ((coOps code) ! counter)
                    else Nothing
          between (min, max) i = i >= min && i <= max

-- Execute an op
exec :: Op -> Action ()
exec Nop = do
    liftIO $ print "pass"
    incTopFrameCounter
exec (Binary binOp) = do
    left <- popStack
    right <- popStack
    result <- (typeFunc $ oType left) left right
    pushStack result
    incTopFrameCounter
    where
        typeFunc = case binOp of
                Add -> tAdd
                Subtract -> tSubtract
                Divide -> tDivide
                Multiply -> tMultiply
                otherwise -> error $ "unknown binary operation"

exec Return = do
    popFrame
    incTopFrameCounter
exec (LoadConst i) = do
    pushConst i
    incTopFrameCounter
exec op = error $ "unexpected operation: " ++ (show op)

pushConst idx = do
    maybeFrame <- topFrame
    case maybeFrame of
        Just frame -> do
            case Map.lookup idx (coConstants $ fCode frame) of
                Just (CInteger i) -> do
                    newIntegerObject i >>= pushStack
                Just CNone -> do
                    newNoneObject >>= pushStack
                otherwise -> error $ "expecting constant: " ++ (show idx)
        otherwise -> error "expecting frame"

-- Step through the current frame.  Return False if we're out of instructions.
step :: Action Bool
step = do
    mOp <- nextOp
    case mOp of
        Just op -> do
            exec op
            return True
        Nothing -> return False

main = do
    vm <- emptyVM
    State.evalStateT runProgram vm
    return ()

