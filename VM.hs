-- Virtual Machine/Interpreter for a dictionaryish language (think Javascript or Python)

module Main where

import qualified Data.Map as Map
import qualified Control.Monad.State as State
import Control.Monad
import Control.Monad.Trans
import Data.Dynamic
import Data.IORef
import Data.Array

type Key = Object
type KeyString = String
type Val = Object
type Self = Object
type IdNumber = Int
type Result = Object
type GetAttr = Self -> KeyString -> Action Result
type SetAttr = Self -> KeyString -> Val -> Action Result
type GetID = Self -> Action IdNumber
type IsEqual = Object -> Object -> Action Result
type Comparator = Object -> Object -> Action Ordering
type FnArgs = [Object]

type BinaryFn = Object -> Object -> Action Object
type UnaryFn = Object -> Action Object

type Call = FnArgs -> Action Object

data Object = Object {
    oIdNumber :: IdNumber,
    oType :: Type,
    oValue :: Value
} deriving (Show)



type Dictionary = Map.Map Int [Object]
type MutableDictionary = IORef Dictionary
type List = [Object]
type MutableList = IORef List

type LVal = String
type ExceptionObject = Object
type ExceptionType = Object

data Frame = Frame { 
    fScope :: [Map.Map LVal Object],
    fCode :: Code,
    fCounter :: Int
} 

-- Program code is just an array of operations
type Code = Array Int Op 

data Value = ValString String
    | ValInt Int
    | ValDouble Double
    | ValDict Dictionary
    -- | ValMutableDict MutableDictionary
    | ValList List
    -- | ValMutableList MutableList
    -- | ValFn Call
    | ValDynamic Dynamic
    deriving (Show)

data Type = Type {
    tAdd :: BinaryFn, -- Function
    tId :: Int, -- Type ID 
    tName :: String
} 

instance Show Type where 
    show (Type {tId=i}) = "Type {tId=" ++ (show i) ++ "}" 

data Op = Stop
    | Jump Int
	| Pop
	| PopTwo
	| RotThree
	| DupTop
	| RotFour
	| Nop
	| Convert
	| ListAppend
	| Binary BinOp
    | Unary UnOp
	| Print
	| Slice Int
    | DeleteSlice Int
    | StoreSlice Int
    | Return
    deriving (Ord, Eq, Show)

data UnOp = Positive | Negative | Not | Invert
    deriving (Ord, Eq, Show)
data BinOp = Add | Subtract | Divide | Multiply | Power
    deriving (Ord, Eq, Show)

data VM = VM {
    vmFrames :: [Frame],
    vmStack :: [Object],
    vmObjectCounter :: Counter
}

type Counter = IORef Int

typeNumObject = 0 
typeNumInt = 10
typeNumString = 11

intType = Type {
    tId = typeNumInt,
    tName = "int",
    tAdd = addInt
}

addInt :: BinaryFn
addInt (Object {oValue=(ValInt i)}) (Object {oValue=(ValInt j)}) = newIntObject (i + j)
addInt left right = unexpectedType left right

--unexpectedType :: Object -> Object -> Action Object
unexpectedType obj1 obj2 = error $ "bad type"

newCounter initial = newIORef initial
incCounter c = atomicModifyIORef c (\x -> (x + 1, x))

emptyCode = array (0, 0) [(0, Return)]

emptyVM = do 
    c <- newCounter 1000
    return $ VM [] [] c

pushFrame frame = State.modify pushFrame'
    where 
        pushFrame' i = i { vmFrames = frame : (vmFrames i) }

popFrame :: Action ()
popFrame = State.modify $ \i@(VM{vmFrames=xs}) -> i{vmFrames=tail xs}

pushStack :: Object -> Action ()
pushStack o = State.modify $ \vm@(VM {vmStack=xs}) -> vm {vmStack=o:xs}

popStack :: Action Object
popStack = do
    vm <- State.get
    let item = head $ vmStack vm
    State.put $ vm { vmStack = tail $ vmStack vm }
    return item

emptyFrame = Frame { fCode = undefined, fCounter = 0, fScope = [] }

nopFrame = emptyFrame {fCode = emptyCode}

-- A very small program
nopProgram = do
    pushFrame nopFrame
    run

-- Program which adds two numbers together
onePlusOneProgram = do 
   -- Push two 1's onto the stack
   (newIntObject 1) >>= pushStack
   (newIntObject 1) >>= pushStack
   -- Add them together
   pushFrame $ emptyFrame {fCode = array (0,1) [(0, Binary Add), (1, Return)]}
   -- Run it!
   run
   -- Print the stack (should be 1 now)
   printStack

-- Print the current stack for debugging
printStack = do
    liftIO $ putStrLn "Stack:" 
    i <- State.get
    forM_ (vmStack i) printObject 

-- Print an object for debugging
printObject o = liftIO $ putStrLn $ "<Object value=\"" ++ (show $ oValue o) ++ "\">"


newObject value valueType = do
    objectID <- nextObjectID 
    return $ Object objectID valueType value
  
nextObjectID :: Action Int
nextObjectID = do
    vm <- State.get
    liftIO $ incCounter $ vmObjectCounter vm

newIntObject :: Int -> Action Object
newIntObject i = newObject (ValInt i) intType

type Action a = State.StateT VM IO a

run :: Action ()
run = while step

while m = ifM m (while m) (return ())

ifM mBool left right = do 
    t <- mBool
    if t then left else right

modifyTopFrame f = State.modify (modifyTopFrame' f)
    where 
        modifyTopFrame' f vm@(VM {vmFrames = []})     = vm 
        modifyTopFrame' f vm@(VM {vmFrames = (x:xs)}) = vm {vmFrames = (f x) : xs}

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
            if between (bounds code) counter 
                    then Just (code ! counter)
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
                otherwise -> error $ "unknown binary operation"
exec Return = do
    popFrame
    incTopFrameCounter
exec op = error $ "unexpected operation: " ++ (show op)

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
    State.evalStateT onePlusOneProgram vm
    return ()
