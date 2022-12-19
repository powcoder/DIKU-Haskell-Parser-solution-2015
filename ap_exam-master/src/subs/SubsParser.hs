https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module SubsParser (
  ParseError(..),
  parseString,
  parseFile
) where


{----- GRAMMAR -----

Program ::= Stms

Stms ::= ϵ
      |  Stm `;` Stms

Stm  ::= `var` Ident AssignOpt
      |  Expr

AssignOpt  ::= ϵ
            |  `=` Expr1

Expr       ::= Expr1 `,` Expr
            |  Expr1

Expr1      ::= Expr2
            |  Expr2 `+` Expr1
            |  Expr2 `-` Expr1
            |  Expr2 `*` Expr1
            |  Expr2 `%` Expr1
            |  Expr2 `<` Expr1
            |  Expr2 `===` Expr1

Expr2     ::=  Number
            |  String
            |  `true`
            |  `false`
            |  `undefined`
            |  Ident AfterIdent
            |  `[` Exprs `]`
            |  `[` `for` `(` Ident `of` Expr1 `)` ArrayCompr Expr1 `]`
            |  `(` Expr `)`

AfterIdent ::= ϵ
            |  `=` Expr1
            |  FunCall

FunCall    ::= `.` Ident FunCall
            |  `(` Exprs `)`

Exprs      ::= ϵ
            |  Expr1 CommaExprs

CommaExprs ::= ϵ
            |  `,` Expr1 CommaExprs

ArrayCompr  ::= ϵ
             |  `if` `(` Expr1 `)` ArrayCompr
             |  `for` `(` Ident `of` Expr1 `)` ArrayCompr

-----}

import Control.Applicative ( Alternative((<|>)) )
import Data.Char

import SubsAst
import SimpleParse


data ParseError = ParseError String
                deriving (Show, Eq)

-- Greedy chainl1
-- Almost identical to chainl1 in SimpleParse, but never considers the case
-- without the operator if an operator is found.
gchainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `gchainl1` op   = do a <- p
                       rest a
                   where
                      rest a = do f <- op
                                  b <- p
                                  rest (f a b)
                               <++ return a

keywords :: [String]
keywords = ["var", "true", "false", "undefined", "for", "of", "if"]

identParser :: Parser Ident
identParser = do
    s <- token $ munch1 $ \ c -> isLetter c || c == '_' || isDigit c
    if firstIsDigit s || s `elem` keywords then reject else return s
    where
        firstIsDigit (c:_) = isDigit c
        firstIsDigit []    = False


numberParser :: Parser Expr
numberParser = do
    neg <- spaces >> option (char '-')
    n <- munch1 isDigit
    let number = read n in  -- Not completely safe as read may crash
        if number > 99999999 then reject
                        else
                            case neg of
                                Just _  -> return $ Number $ -number
                                Nothing -> return $ Number number


exprsParser :: Parser [Expr]
exprsParser = expr1Parser `sepBy` schar ','


afterIdentParser :: Ident -> Parser Expr
afterIdentParser ident =
    do expr <- schar '=' >> expr1Parser; return $ Assign ident expr
    <|> funCallParser ident
    <|> return (Var ident)
    where
        funCallParser :: Ident -> Parser Expr
        funCallParser i1 = do
                           i2 <- schar '.' >> identParser
                           funCallParser (i1 ++ "." ++ i2)
                       <|> do
                           exprs <- schar '(' >> exprsParser
                           _ <- schar ')'
                           return $ Call i1 exprs


arrayComprParser :: Parser (Maybe ArrayCompr)
arrayComprParser = option (
    do
        e <- symbol "if" >> schar '(' >> expr1Parser
        a <- schar ')' >> arrayComprParser
        return $ ArrayIf e a
    <|> do
        i <- symbol "for" >> schar '(' >> identParser
        e <- symbol "of" >> expr1Parser
        a <- schar ')' >> arrayComprParser
        return $ ArrayForCompr (i, e, a))


expr1Parser :: Parser Expr
expr1Parser = expr2
    `gchainl1` mulOp
    `gchainl1` addOp
    `gchainl1` lessOp
    `gchainl1` eqOp
    where
        mulOp :: Parser (Expr -> Expr -> Expr)
        mulOp  = do { _ <- schar '*';    return $ expectTwo $ Call "*" }
             <|> do { _ <- schar '%';    return $ expectTwo $ Call "%" }
        addOp  = do { _ <- schar '+';    return $ expectTwo $ Call "+" }
             <|> do { _ <- schar '-';    return $ expectTwo $ Call "-" }
        lessOp = do { _ <- schar '<';    return $ expectTwo $ Call "<" }
        eqOp   = do { _ <- symbol "==="; return $ expectTwo $ Call "===" }
        expectTwo :: ([a] -> a) -> a -> a -> a
        expectTwo f a b = f (a:[b])

        expr2 = numberParser
            <|> do
                s <- schar '\'' >> munch (/= '\'')
                _ <- schar '\''
                return $ String s
            <|> do { _ <- symbol "true"; return TrueConst }
            <|> do { _ <- symbol "false"; return FalseConst }
            <|> do { _ <- symbol "undefined"; return Undefined }
            <|> do { i <- identParser; afterIdentParser i }
            <|> do
                exprs <- schar '[' >> exprsParser
                _     <- schar ']'
                return $ Array exprs
            <|> do { _ <- schar '('; e <- exprParser; _ <- schar ')'; return e}
            <|> do
                _ <- schar '[' >> symbol "for" >> schar '('
                i <- identParser
                e1 <- symbol "of" >> expr1Parser
                a <- schar ')' >> arrayComprParser
                e2 <- expr1Parser
                _ <- schar ']'
                return $ Compr (i, e1, a) e2


exprParser :: Parser Expr
exprParser = expr1Parser `chainl1` commaOp
    where
        commaOp = do { _ <- schar ','; return Comma }

assignOptParser :: Parser (Maybe Expr)
assignOptParser = option $ schar '=' >> expr1Parser

stmtParser :: Parser Stm
stmtParser = do
                i <- symbol "var" >> identParser
                a <- assignOptParser
                return $ VarDecl i a
         <|> do { e <- exprParser; return $ ExprAsStm e}

stmtsParser :: Parser [Stm]
stmtsParser = do
                  stmt  <- stmtParser
                  _     <- schar ';'
                  stmts <- stmtsParser
                  return $ stmt:stmts
         <|> return []

programParser :: Parser Program
programParser = do {statements <- stmtsParser; return $ Prog statements}

parseString :: String -> Either ParseError Program
parseString input = let ret = do { cs <- programParser; _ <- spaces; return cs } in
    case parseEof ret input of
    [(prog, "")] -> return prog
    f -> Left $ ParseError $ show f  -- TODO: Better error message

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = parseString <$> readFile path
