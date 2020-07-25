{
{-
    Un frammento di linguaggio Haskell formato da:
     - identificatori
     - applicazione 
     - costrutti "let" e "case"
-}

module Main(main) where
import ScannerHskP
}

%name parser_hsk
%tokentype { Token }
%error { parseError }

%token
    'let'       { ReservedWord Let          }
    '='         { ReservedWord ScannerHskP.Assignment   }
    ';'         { ReservedWord DotComma     }
    'in'        { ReservedWord In           }
    'case'      { ReservedWord Case         }
    'of'        { ReservedWord Of           }
    '->'        { ReservedWord RightArrow   }
    'otherwise' { ReservedWord Otherwise    }

    '('         { BracketOpen }
    ')'         { BracketClose }

    iden        { Identificator $$   }
    num_const   { NaturalConstant $$ }
    -- code        { Code $$ }

%right '='
%nonassoc '->'
%left ';'
%right 'let' 'case'
%right 'in' 'of'

%%

Exp : iden                          { Iden $1 }
    | num_const                     { Num $1 }
    | iden FunArgs                  { AppFun $1 $2  }
    | 'let' Assignments 'in' Exp    { LetIn $2 $4   }
    | 'case' Exp 'of' Cases         { CaseOf $2 $4  }
    | BracketExp                    { $1 }

BracketExp : '(' Exp ')'                 { $2 }
           | '(' BracketExp ')'          { $2 }

FunArgs : Exp           { $1 : [] }
        | Exp FunArgs   { $1 : $2 }

SingleAssignment : iden '=' Exp                     { Main.Assignment $1 $3 }

Assignments      : SingleAssignment                 { $1 : [] }
                 | SingleAssignment ';' Assignments { $1 : $3 }

SingleCase : iden  '->' Exp        { CaseIden $1 $3   }
           | num_const '->' Exp    { CaseNum $1 $3    }

Cases      : SingleCase            { $1 : [] }
           | 'otherwise' '->' Exp  { (OtherwiseCase $3) : [] }
           | SingleCase ';' Cases  { $1 : $3 }

{

data Exp =  Iden String | Num Int | AppFun String [Exp] | LetIn [Assignment] Exp | CaseOf Exp [Case] deriving(Eq, Show)

data Assignment = Assignment String Exp deriving(Eq, Show)
data Case = CaseIden String Exp | CaseNum Int Exp | OtherwiseCase Exp deriving(Eq, Show)

parseError :: [Token] -> a
parseError t = error $ "Parse error. " ++ show t

scanAndParse :: String -> Exp
scanAndParse = parser_hsk . scanTokens

scanParseAndPrint :: String -> IO ()
scanParseAndPrint = print . scanAndParse

main :: IO ()
main = getContents >>= (\s -> sequence_ ( (lines s) >>= (return . scanParseAndPrint ) )

}