module BIL where

import Text.Parsec.ByteString
import Text.Parsec
import Text.Parsec.Token (GenLanguageDef(..))
import qualified Text.Parsec.Token as Tok
import Data.ByteString (ByteString)
import Control.Monad.Identity
import Text.Parsec.Expr

data Endian = Big | Little deriving Show
data Var = V {vName :: String, vTy :: (Maybe Type)} deriving Show
data Type = Reg Int
          | TMem Type
          | Arr Type Type deriving Show
data BinOp = Plus
           | Minus
           | Times
           | Divide
           | SDivide
           | Mod
           | SMod
           | LShift
           | RShift
           | ARShift
           | And
           | Or
           | Xor
           | Eq
           | NEq
           | Lt
           | LtE
           | SLt
           | SLtE deriving Show
data UnOp = Neg | Not deriving Show
data Exp = 
    Load { bMem    :: Exp
         , bIdx    :: Exp
         , bEndian :: Endian
         , bType   :: Type
         }
  | Store { bMem    :: Exp
          , bIdx    :: Exp
          , bEndian :: Endian
          , bPay    :: Exp
          , bType   :: Type
          }
  | BinOp { bOp :: BinOp
          , bI1 :: Exp
          , bI2 :: Exp
          }
  | UnOp { bUOp :: UnOp
         , bI1  :: Exp
         }
  | Lab String
  | Int { bInt :: Integer
        , bType :: Type
        }

  | Let { bVar   :: Var
        , bBound :: Exp
        , bExp   :: Exp
        }
  | Var { bVar   :: Var
        }
  | Unknown { bMsg :: String
            , bType :: Type
            }
  | Ite { bCond :: Exp
        , bThen :: Exp
        , bElse :: Exp
        }
  | Extract { bHBits :: Int
            , bLBits :: Int
            , bExp   :: Exp
            }
  | Concat { bHReg :: Exp
           , bLReg :: Exp
           }
  | Cast { bCast :: Cast
         , bType :: Type
         , bExp  :: Exp
         } deriving Show
data Cast = Pad | Extend | High | Low deriving Show
type Attrs = [Attr]
data Attr = Asm String
          | Address Integer
          | Set String
          | StrAttr String deriving Show

data Stmt = 
    Move { sDest  :: Var
         , sExp   :: Exp
         , sAttrs :: Attrs
         }
  | Jmp { sTarget :: Exp
        , sAttrs  :: Attrs
        }
  | CJmp { sCond       :: Exp
         , sThenTarget :: Exp
         , sElseTarget :: Exp
         , sAttrs      :: Attrs
         }
  | SLabel { sLabel :: Label
           , sAttrs :: Attrs
           }
  | Halt { sRetV :: Exp
         , sAttrs :: Attrs
         }
  | Assert { sAss :: Exp
           , sAttrs :: Attrs
           }
  | Special { sMsg :: String
            , sAttrs :: Attrs
            } deriving Show
data Label = StrLab String
           | AddrLab Integer deriving Show

astStmtParser :: Parser Stmt
astStmtParser = jmpP <|> cjmpP <|> haltP <|> assertP <|> specialP <|> labelP <|> movP


bilDef :: GenLanguageDef ByteString () Identity
bilDef = LanguageDef
   {commentStart = "/*",
    commentEnd = "*/",
    commentLine = [],
    nestedComments = True,
    identStart = letter <|> char '_',
    identLetter = alphaNum <|> char '_',
    opStart = oneOf ":=+-*/%&^|<>!~",
    opLetter = oneOf ":=&|<>",
    reservedNames = ["label", "special", "assert", "halt", "cjmp", "jmp"],
    reservedOpNames = ["="],
    caseSensitive = True}

bilTokens :: Tok.GenTokenParser ByteString () Identity
bilTokens = Tok.makeTokenParser bilDef

reserved      = Tok.reserved      bilTokens
stringLiteral = Tok.stringLiteral bilTokens
identifier    = Tok.identifier    bilTokens
reservedOp    = Tok.reservedOp    bilTokens
parens        = Tok.parens        bilTokens
integer       = Tok.integer       bilTokens
natural       = Tok.natural       bilTokens
brackets      = Tok.brackets      bilTokens
comma         = Tok.comma         bilTokens
whiteSpace    = Tok.whiteSpace    bilTokens

astProgParser = do
  v <- many astStmtParser
  eof
  return v

jmpP = do
  reserved "jmp"
  e  <- astExprParser
  as <- attrsParser
  return $ Jmp e as
cjmpP = do
  reserved "cjmp"
  i  <- astExprParser
  comma
  t  <- astExprParser
  comma
  e  <- astExprParser
  as <- attrsParser
  return $ CJmp i t e as
haltP = do
  reserved "halt"
  e  <- astExprParser
  as <- attrsParser
  return $ Halt e as
assertP = do
  reserved "assert"
  e  <- astExprParser
  as <- attrsParser
  return $ Assert e as
specialP = do
  reserved "special"
  s  <- stringLiteral
  as <- attrsParser
  return $ Special s as
labelP = labelPLabel <|> labelPAddr
labelPLabel = do
  reserved "label"
  l  <- identifier --TODO this is not specified how to do it right :(
  as <- attrsParser
  return $ SLabel (StrLab l) as
labelPAddr = do
  reserved "addr"
  n  <- natural
  as <- attrsParser
  return $ SLabel (AddrLab n) as
movP = do
  i  <- var
  reservedOp "="
  e  <- astExprParser
  as <- attrsParser
  return $ Move i e as

var = do
  i  <- identifier
  (do reservedOp ":"
      ty <- typeP
      return $ V i (Just ty)) <|> (return $ V i Nothing)

astExprParser = expr
expr = buildExpressionParser opTable term

idxP = brackets $ do
  idx <- expr
  comma
  end <- endianP
  return (idx, end)

loadP = do
  (idx, end) <- idxP
  reservedOp ":"
  ty <- typeP
  return $ \e -> Load e idx end ty

storeP = do
  reserved "with"
  (idx, end) <- idxP
  reservedOp ":"
  ty <- typeP
  reservedOp "="
  v  <- expr
  return $ \e -> Store e idx end v ty

term = do
  b <- bTerm
  is <- many $ loadP <|> storeP
  return $ foldl (flip ($)) b is

bTerm = do
  parens expr
  <|>
  bool
  <|>
  (do n <- integer
      reservedOp ":"
      t <- typeP
      return $ Int n t)
  <|>
  fmap Lab stringLiteral
  <|>
  extractP
  <|>
  concatP
  <|>
  castP
  <|>
  unknownP
  <|>
  fmap Var var

unknownP = do
  reserved "unknown"
  msg <- stringLiteral
  reservedOp ":"
  ty  <- typeP
  return $ Unknown msg ty

castP = do
  c <- cast
  reservedOp ":"
  t <- typeP
  e <- parens expr
  return $ Cast c t e
cast =
  (do reserved "pad"
      return Pad)
  <|>
  (do reserved "extend"
      return Extend)
  <|>
  (do reserved "high"
      return High)
  <|>
  (do reserved "low"
      return Low)

extractP = do
  reserved "extract"
  reservedOp ":"
  h <- natural
  reservedOp ":"
  l <- natural
  reservedOp ":"
  e <- brackets expr
  return $ Extract (fromIntegral h) (fromIntegral l) e

concatP = do
  reserved "concat"
  reservedOp ":"
  l <- brackets expr
  r <- brackets expr
  return $ Concat l r

bool =
  (do reserved "false"
      return $ Int 0 (Reg 1))
  <|>
  (do reserved "true"
      return $ Int 1 (Reg 1))


binary :: String -> BinOp -> Assoc -> Operator ByteString () Identity Exp
binary name k = Infix $ do reservedOp name
                           return $ BinOp k
prefix :: String -> UnOp -> Operator ByteString () Identity Exp
prefix name k = Prefix $ do reservedOp name
                            return $ UnOp k

opTable = [[prefix "-"   Neg,
            prefix "~"   Not
           ]
          ,[binary "*"   Times   AssocLeft
           ,binary "/"   Divide  AssocLeft
           ,binary "$/"  SDivide AssocLeft
           ,binary "%"   Mod     AssocLeft
           ,binary "$%"  SMod    AssocLeft
           ]
          ,[binary "+"   Plus    AssocLeft
           ,binary "-"   Minus   AssocLeft
           ]
          ,[binary "<<"  LShift  AssocLeft
           ,binary ">>"  RShift  AssocLeft
           ,binary "$>>" ARShift AssocLeft
           ]
          ,[binary "<"   Lt      AssocLeft
           ,binary "$<"  SLt     AssocLeft
           ,binary "$<=" SLtE    AssocLeft
           ,binary "<="  LtE     AssocLeft
           ]
          ,[binary "=="   Eq      AssocLeft
           ,binary "<>"  NEq     AssocLeft
           ]
          ,[binary "&"   And     AssocLeft]
          ,[binary "^"   Xor     AssocLeft]
          ,[binary "|"   Or      AssocLeft]
          ]

--TODO this parse is ambiguous, but since I assume we won't
--want to index arrays by arrays very often, I've picked
--the direction that means nested array types will always
--be in the value.
typeP = do
  t <- typeP'
  (do char '?'
      fmap (Arr t) typeP) <|> (return t)

typeP' =
  (do reserved "bool"
      return $ Reg 1)
  <|>
  (do char 'u'
      n <- natural
      return $ Reg $ fromIntegral n)
  <|>
  (do char '?'
      t <- typeP
      return $ TMem t)

endianP =
  (do reserved "e_little"
      return Little)
  <|>
  (do reserved "e_big"
      return Big)

attrsParser = many attrP

attrP = asmP <|> addrP <|> liveoutP <|> strAttrP

asmP = do
  reserved "@asm"
  fmap Asm stringLiteral
addrP = do
  reserved "@address"
  fmap Address natural
liveoutP = do
  reserved "@set"
  fmap Set stringLiteral
strAttrP = do
  reserved "@str"
  fmap StrAttr stringLiteral

-- TODO write these parsers. We don't really use these attrs atm
-- <|> ctxP <|> tidP <|> initROP <|> synthP
