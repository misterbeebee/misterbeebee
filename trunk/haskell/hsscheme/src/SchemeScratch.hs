-- older versions of function definitions


parseHexNumber1 :: Parser LispVal
parseHexNumber1 = do
  string "#x"
  (many1 hexDigit) >>= (return . (Number . (fromReadSResult . readHex)))
   
makeParseTaggedNumber :: String -> (Parser Char) -> ReadS Integer -> Parser LispVal
makeParseTaggedNumber tag digitParser digitStrReader = do
  string tag
  (many1 digitParser) >>= (return . (Number . (fromReadSResult . digitStrReader)))

parseHashTaggedHexNumber = makeParseTaggedNumber "#x" hexDigit readHex
parseHashTaggedOctNumber = makeParseTaggedNumber "#o" octDigit readOct
parseHashTaggedBinNumber = makeParseTaggedNumber "#b" binaryDigit readBin
parseHashTaggedDecNumber = makeParseTaggedNumber "#d" digit readDec

{- Use 'try' to backtrack: if an earlier parser fails, push back the consumed
text of the failed parse. try has performace problems due to backtracking /
exponential explosion. If performance becomes a bottleneck, or as a design
challenge (Ex 3.4#3), left-factor the parser with a more tree-like structure ('#' >> Bin <|> Oct ...)
-}
parseNumber1 :: Parser LispVal
parseNumber1 = try parseUntaggedNumber 
  <|> parseHashTaggedNumber

-- backtracking definition. 'try' is required to unconsume '#' after misparse
parseHashTaggedNumber :: Parser LispVal
parseHashTaggedNumber =  do
  try parseHashTaggedBinNumber 
    <|> try parseHashTaggedHexNumber
    <|> try parseHashTaggedOctNumber 
    <|> try parseHashTaggedDecNumber 



-- "read" is a difference kind of parser, not part of the Parsec monad, so liftM pushes the read-and-construct-Number function into the Parser monad.
parseUntaggedNumber1 :: Parser LispVal
parseUntaggedNumber1 = liftM (Number . read) $ many1 digit

-- equivalently:
parseUntaggedNumber2 :: Parser LispVal
parseUntaggedNumber2 = do
  numberString <- many1 digit
  return $ (Number . read) numberString


-- equivalently:
parseUntaggedNumber :: Parser LispVal
parseUntaggedNumber = (many1 digit) >>= (return . (Number . read))



-- Lists
parseProperListBody :: Parser LispVal
parseProperListBody = liftM List $ do
  theList <- sepBy parseExpr spaces
  return theList

parseDottedListBody :: Parser LispVal
parseDottedListBody = do
  headVal <- endBy parseExpr spaces
  tailVal <- char '.' >> spaces >> parseExpr 
  return $ DottedList headVal tailVal
parseAnyList1 :: Parser LispVal
parseAnyList1 = trace "parseAnyList" $ do
  char '('
  -- TODO: (Ex 3.4#3), left-factor the parser with a more tree-like structure ('#' >> Bin <|> Oct ...)
  x <- (try parseProperListBody) <|> parseDottedListBody
  char ')'
  return x


