{-# GHC_OPTIONS -fglasgow-exts #-} 
module Main where

import System.Environment
import System.Random
import Control.Monad.Error

-- Note: Doc linked from Scheme in 48 hrs is from an older version of library, that has different module structure
import Text.ParserCombinators.Parsec
-- nee TokenParser
import qualified Text.ParserCombinators.Parsec.Token  as PT 
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Monad (liftM)
import Control.Monad ( (=<<) )
import Numeric 
import Char 
import qualified Debug.Trace as DT
import IO hiding (try)

import Data.IORef


-- State management
type EnvRef = IORef [(String, IORef LispVal)]
nullEnv :: IO EnvRef
nullEnv = newIORef []

isTracing = False

trace = if isTracing then DT.trace else (\_ y -> y)

--  Steal a float parser from Parsec's parser-generator geerator,
-- instead of carefully writing our own. (Cheating!)
tokenParser = PT.makeTokenParser (haskellDef)
tpFloat = PT.float tokenParser
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal 
             | Number Integer
             | String String
             | Bool Bool
             | Character Char -- Ex. 3.3.
             | Float Double -- Ex. 3.3.
             | LError LispError
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), 
                      body :: [LispVal], closure :: EnvRef}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Error LispError where
     -- provides throwError, catchError
     noMsg = Default "An error has occurred"
     strMsg = Default

-- Either is poorly-named, has implicit semantics: Left->error; Right->value 
type ThrowsError = Either LispError  -- partially applied type-constructor

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- Monad Transformer to interleave Error and IO
-- ErrorT mixes the Error intance (LispError) with another monad instance (IO)
-- IOThrowsError does uses a well-named 'ErrorT' in place of the poorly-named "Either" 
-- IOThowsError <==> "[Inner][Outer]"
type IOThrowsError = ErrorT LispError IO

-- normal 'lift' gives (IO a -> OuterM IO a)
-- liftThrows gives ( OuterM a -> OuterM IO a)  (really, liftAndSlide :-) )
-- liftThrows :: MonadError m => Either e a -> m e      -- more general
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err    -- change Left(error) into a thrown error
liftThrows (Right val) = return val       -- change Right(value) into an IO val

-- "unlift"  from (Error IO) to IO
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- Scheme Variables
isBound :: EnvRef -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: EnvRef -> String -> IOThrowsError LispVal
getVar envRef var =  do env       <- (liftIO . readIORef) envRef
                        let value = (lookup var env)
                        maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                              (liftIO . readIORef)
                              value

-- sets a value for a var, and returns the *new* value
setVar :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env        <- (liftIO . readIORef) envRef
                             let exists =  (lookup var env)
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                                   (liftIO . (flip writeIORef value))
                                   exists
                             return value


-- Cleaned up a bit from tutorial's version
defineVar :: EnvRef -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    let setOrCreateVar = if alreadyDefined then setVar else createVar
    setOrCreateVar envRef var value
   where
    createVar envRef var value =  liftIO $ do
      valueRef <- newIORef  value
      env      <- readIORef envRef
      writeIORef envRef ((var,valueRef) : env)
      return value

-- Create a new env with (old env ++ new bindings)
makeLocalEnv :: EnvRef -> [(String, LispVal)] -> IO EnvRef
makeLocalEnv envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM makeBinding bindings)
          makeBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)


------- Parsers
-- http://www.cs.uu.nl/~daan/download/parsec/parsec.html

-- tutorial calls this spaces, which hides Parsec.spaces and causes
-- maddening bugs when we forget :-( Be responsible and choose a
-- non-conflicting, and more informative, name
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- Extract the first parse from a readS structure, or die horribly
-- This is completely unsafe!
fromReadSResult :: [(Integer,String)] -> Integer
fromReadSResult = fst . head

-- components of tokens. Apparently, these are valid inside lisp atoms
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" 

----- Parsers for tokens
parseEscapedChar :: Parser Char
parseEscapedChar = do
  (oneOf "\\" ) 
  code <- (oneOf "\"nrt\\") 
  return $ case code of 
    '"' -> '"'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    '\\' -> '\\'
 
makeParseBaseTaggedNumber :: Char -> (Parser Char) -> ReadS Integer -> Parser LispVal
makeParseBaseTaggedNumber baseTag digitParser digitStrReader = do
  char baseTag 
  (many1 digitParser) >>= (return . (Number . (fromReadSResult . digitStrReader)))

parseBaseTaggedHexNumber = makeParseBaseTaggedNumber 'x' hexDigit readHex
parseBaseTaggedOctNumber = makeParseBaseTaggedNumber 'o' octDigit readOct
parseBaseTaggedBinNumber = makeParseBaseTaggedNumber 'b' binaryDigit readBin
parseBaseTaggedDecNumber = makeParseBaseTaggedNumber 'd' digit readDec

-- adapted from Numeric readHex
-- adapted from Char isHexDigit
binaryDigit = oneOf $ "01"
isBinaryDigit x = x == '0' || x == '1' 
readBin = readInt 2 isBinaryDigit digitToInt

parseString :: Parser LispVal
parseString = do 
                char '"'
                x <- many ((noneOf "\"\\")  <|> parseEscapedChar)
                char '"'
                return $ String (traceF0 "parseString" x)

parseHashToken = 
  ((char '#') >> (
     (char 't' >> (return $ Bool True))  <|>
     (char 'f' >> (return $ Bool False)) <|>
     (char '\\' >> (parseCharacter)) <|>
     parseBaseTaggedNumber 
  ))

-- Partial support for Character literal  -- single-charater literal, not character name
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
parseCharacter :: Parser LispVal
parseCharacter = liftM (Character . (\x -> trace ("Character " ++ [x]) x)) anyChar  

parseLiteralAtom = do
    first <- (letter <|> symbol)
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ Atom atom


{- Note the contortions we do to trace read.... we need to make trace's
message depend on the the result of a calculation, or else it will only print its
message once and then memoize the result of rendering a message.
-}
parseUntaggedNumber :: Parser LispVal
parseUntaggedNumber = (many1 digit) >>= (return . (Number . (traceF1 "number" read)))

traceF0 msg f = let x = f in trace (msg ++ " " ++ show x) x
traceF1 msg f arg1 = let x = f arg1 in trace (msg ++ " " ++ show x) x
traceF2 msg f arg1 arg2 = let x = f arg1 arg2 in trace (msg ++ " " ++ show x) x

-- Note that "Number" is actually an Integer. Floats are a separate Lisp type.

parseNumber = trace "parseNumber"  $
      parseUntaggedNumber 

-- Non-backtracking definition
parseBaseTaggedNumber :: Parser LispVal
parseBaseTaggedNumber =  do
  parseBaseTaggedBinNumber 
    <|> parseBaseTaggedHexNumber
    <|> parseBaseTaggedOctNumber 
    <|> parseBaseTaggedDecNumber 

parseListHead:: Parser [LispVal]
parseListHead = do
  theHead <- spaces >> sepEndBy parseExpr spaces1 
  return $ traceF0 "parseListHead" theHead

data LispListTail = DottedListTail LispVal | ProperListTail 

parseListTail:: Parser LispListTail
parseListTail = trace "parseListTail" $ do
  parseDottedListTail           -- " . <expr>"
  <|> trace "ProperListTail" (return ProperListTail) 


parseDottedListTail :: Parser LispListTail
parseDottedListTail = (trace "Try parseDottedListTail" ) $ liftM DottedListTail $
  char '.' >> spaces1 >> (liftM $ trace "DottedListTail") parseExpr >>= (\x -> spaces >> return x)

parseAnyList :: Parser LispVal
parseAnyList = trace "parseAnyList" $ do
  char '('
  -- TODO: (Ex 3.4#3), left-factor the parser with a more tree-like structure ('#' >> Bin <|> Oct ...)
  headVal <- parseListHead 
  tailVal <- parseListTail
  let x = case tailVal of 
                ProperListTail   -> List headVal 
                DottedListTail t -> DottedList headVal t
  char ')'
  return x

{- Use 'try' to backtrack: if an earlier parser fails, push back the consumed
text of the failed parse. try has performace problems due to backtracking /
exponential explosion. If performance becomes a bottleneck, or as a design
challenge, refactor the parser with a more tree-like structure ('#' -> parse
special literals, others -> parse atom, etc).  
-}

parseExpr :: Parser LispVal
parseExpr = 
  parseString
  <|> parseHashToken
  <|> parseLiteralAtom
  <|> parseAnyList -- lists and dotted lists are ambigious until the end is reached.
  <|> parseQuoted
 
  -- Since "5.6" will parse as a Number (and ignore the leftover), 
  -- try parse Float before Number. In a better world, we could collect
  -- the Integer portion, and then choose between Number and Float
  -- based on whether a decimal appears. Instead, backtrack with'try'
  <|> (try parseFloat)  
  <|> parseNumber 

parseFloat :: Parser LispVal
parseFloat = trace "parseFloat" $ liftM Float tpFloat -- Totally cheat by using Parsec's builtin float parser.

parseQuoted :: Parser LispVal
parseQuoted = trace "parseQuoted" $ do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


--Evaluation

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal


eval :: EnvRef -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Character _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval envRef (Atom id) = getVar envRef id   -- get variable binding for name

-- Special forms
eval _ (List (Atom "quote" : args)) = liftThrows $ primQuote args -- special 'form'
eval envRef (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval envRef pred
       case result of
         Bool False -> eval envRef alt
         otherwise  -> eval envRef conseq

eval envRef (List (Atom "set!" : [(Atom var), value])) = 
  (eval envRef value) >>= setVar envRef var 

eval envRef (List (Atom "define" : [(Atom var), value])) =  
  (eval envRef value) >>= defineVar envRef var 

eval envRef (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc envRef params body >>= defineVar envRef var
eval envRef (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs envRef params body >>= defineVar envRef var
eval envRef (List (Atom "lambda" : List params : body)) =
    makeNormalFunc envRef params body
eval envRef (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs envRef params body
eval envRef (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs envRef [] body
eval envRef (List [Atom "load", String filename]) = 
    -- special form to allow each eval to update the envRef for the next eval
    load filename >>= liftM last . mapM (eval envRef) 

eval envRef (List (functionAtom : args)) = do 
    func <- eval envRef functionAtom
    argVals <- mapM (eval envRef) args
    apply func argVals

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ makeLocalEnv closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ makeLocalEnv env [(argName, List $ remainingArgs)]
              Nothing -> return env 

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [] x] = return x -- tutorial's version is needlessly broad, gives overloading warning
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast -- cleaner than tut
cons [x1, x2] = return $ DottedList [x1] x2 -- non-list at end
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv = eqv_generic eqv

eqv_generic :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqv_generic _ [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv_generic _ [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv_generic _ [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv_generic _ [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv_generic _ [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv_generic eqf [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqf [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv_generic _ [_, _] = return $ Bool False
eqv_generic _ badArgList = throwError $ NumArgs 2 badArgList



primQuote :: [LispVal] -> ThrowsError LispVal
primQuote [a] = return a
primQuote badVal = throwError $ NumArgs 2 badVal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("quote", primQuote),
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
 ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = liftM (Number . foldl1 op) $ mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
{-
-- Non-standard weak-typing: attempt to interpret strings
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
-- Non-standard weak-typing: extract a value from singleton
unpackNum (List [n]) = unpackNum n
-}
unpackNum notNum = throwError $ TypeMismatch "number" notNum


-- Section 6.4
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv_generic equal [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


-- 
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- Incomplete implementation
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename



----- Read-Print iteration (no Eval yet)
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


-- This should be converted to use efficient shows (stream) form
instance Show LispVal where
  show lispVal = case lispVal of
    (String value) -> "String:" ++ value 
    (Number value) -> "Number:" ++ (show value) 
    (Atom value) -> "Atom:" ++ (show value) 
    (Bool value) -> "Bool:" ++ (show value) 
    (Character value) -> "Character:" ++ (show value) 
    (Float value) -> "Float:" ++ (show value) 
    (List value) -> "List:" ++ (show value) 
    (DottedList head tail) -> "DottedList: " ++ (show head) ++ " . " ++ (show tail) 
    (PrimitiveFunc _) -> "<primitive>"
    (Func {params = args, vararg = varargs, body = body, closure = env}) -> 
      "(lambda (" ++ unwords (map show args) ++ 
        (case varargs of 
          Nothing -> ""
          Just arg -> " . " ++ arg) ++ ") ...)" 
    (Port _) -> "<IO port>"
    (IOFunc _) -> "<IO primitive>"


showVal :: LispVal -> String
showVal lispVal = case lispVal of
    String value ->  value 
    Number x -> show x
    Atom x -> x
    Bool x -> if x then "#t" else "#f"
    Character x -> show x
    Float x -> show x
    List x -> "(" ++ unwordsList x ++ ")"
    DottedList head tail -> "(" ++ unwordsList head ++ " . " ++ (showVal tail) ++ ")"
    LError x -> show x
    (PrimitiveFunc _) -> "<primitive>"
    (Func {params = args, vararg = varargs, body = body, closure = env}) -> 
      "(lambda (" ++ unwords (map show args) ++ 
        (case varargs of 
          Nothing -> ""
          Just arg -> " . " ++ arg) ++ ") ...)" 
    (Port _) -> "<IO port>"
    (IOFunc _) -> "<IO primitive>"

unwordsList = unwords . map showVal 

showParseAttempt :: (Parser LispVal) -> String -> IO ()
showParseAttempt parser input = putStrLn $ case parse parser "lisp" input of
    Left err -> "No match: " ++ show err
    Right lispVal -> show lispVal

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: EnvRef -> String -> IO String
evalString envRef expr = runIOThrows $
  (liftM showVal) . (eval envRef) =<<  (liftThrows $ readExpr expr)

evalAndPrint :: EnvRef -> String -> IO ()
evalAndPrint envRef expr =  putStrLn =<< evalString envRef expr 

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

primitiveBindings :: IO EnvRef
primitiveBindings = nullEnv >>= (flip makeLocalEnv $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "hss> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    let (schemeProg : schemeArgs) = args
    env <- primitiveBindings >>= flip makeLocalEnv [("args", List $ map String $ schemeArgs)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String schemeProg])) 
         >>= hPutStrLn stderr


main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args


{-
Todo: http://halogen.note.amherst.edu/~jdtang/scheme_in_48/tutorial/parser.html 
  Exercise: 3.3#7 
  Exercises: 3.4 : backquote, vectors,
  Exercises 4.3: type-testing functions, Scheme funcs symbol->string and string->symbol 
Bookmark: Read section 3.4 onward
  Exercise 6.4: cond, case, string funcs,
-}
