-- ghci parser_exp.hs scanner_exp.hs

{
module Main(main,parser_exp) where
import ScannerExp
}

%name parser_exp 
%tokentype { Token }
%error { parseError }


%token
    num     { NaturalNumber $$ }

    '+'     { Operator Plus     }
    '-'     { Operator Minus    }
    '*'     { Operator Times    }
    '/'     { Operator Divide   }

    '('     { TokenRoundBr Open   }
    ')'     { TokenRoundBr Close  }
    '['     { TokenSquareBr Open  }
    ']'     { TokenSquareBr Close }
    '{'     { TokenGraphBr  Open  }
    '}'     { TokenGraphBr Close  }

%left '+' '-'
%left '*' '/'

%%

Exp : num            {Val $1}
    | Exp '+' Exp    { Operation Plus $1 $3 }
    | Exp '-' Exp    { Operation Minus $1 $3 }
    | Exp '*' Exp    { Operation Times $1 $3 }
    | Exp '/' Exp    { Operation Divide $1 $3 }
    | BracketExp1     { $1 }
    | BracketExp2     { $1 }
    | BracketExp3     { $1 }


    -- per precedenza di parentesi 

BracketExp1 : '(' Exp ')'           { InsideRnd $2 }
            | '(' BracketExp1 ')'   { InsideRnd $2 }

BracketExp2 : '[' BracketExp1 ']'   { InsideSqr $2 }
            | '[' BracketExp2 ']'   { InsideSqr $2 }

BracketExp3 : '{' BracketExp2 '}'   { InsideGrp $2 }
            | '{' BracketExp3 '}'   { InsideGrp $2 }


{

data Exp =  Val Int                     | 
            InsideRnd Exp               | 
            InsideSqr Exp               | 
            InsideGrp Exp               | 
            Operation Operator Exp Exp  
                deriving(Eq, Show)

parseError :: [Token] -> a
parseError t = error $ "Parse error. " ++ show t

scanAndParse :: String -> Exp
scanAndParse = parser_exp . scanTokens

scanAndPrint :: String -> IO ()
scanAndPrint = print . scanAndParse


main :: IO ()
main = getContents >>= (\s -> sequence_ ( (lines s) >>= (return . scanAndPrint )) )
}