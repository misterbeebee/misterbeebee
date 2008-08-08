module Tests.Data.TestBasicHSQL where

import IO
import Database.HSQL as Hsql
import Database.HSQL.SQLite3 as Hsql

simpleDB = "tmp/simple.db"

sqlCreate = "create table if not exists simpletable(mydata)"
sqlInsert = "insert into simpletable values('dogs and cats')"
sqlSelect = "select mydata from simpletable"

--
-- Get Rows routine from David at davblog48
getRows :: Statement -> IO [[String]]
getRows stmt = do
  let fieldtypes = map (\(a,b,c) -> a) $ getFieldsTypes stmt
  rowdata <- collectRows (\s -> mapM (getFieldValue s) fieldtypes ) stmt
  return rowdata

runTestBasicHSQL = do
  putStrLn "Test HSQL"
  tryconn <- try $ Hsql.connect simpleDB ReadWriteMode
  conn <- case tryconn of
            Left _ -> error "Invalid Database Path"
            Right conn -> return conn
  
  -- Run a simple create query
  stmt <- Hsql.query conn sqlCreate
  Hsql.closeStatement stmt
  stmt <- Hsql.query conn sqlInsert
  Hsql.closeStatement stmt
  stmt <- Hsql.query conn sqlSelect
  rows <- getRows stmt
  putStrLn $ "Length rows=" ++ show (length rows)
  mapM_ (\val -> putStrLn $ show val) rows
  Hsql.closeStatement stmt
  Hsql.disconnect conn
-- End of File
