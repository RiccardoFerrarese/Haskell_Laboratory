{
{-
    Un frammento di linguaggio Haskell formato da:
     - identificatori
     - applicazione 
     - costrutti "let" e "case"
-}

module ScannerHskP(runScanner,scanTokens, Token(..),ReservedWord(..)) where
}

%wrapper "basic"

$digit          =   0-9
$alpha          =   [a-z A-Z]

@res_let        = "let"
$res_assign     = \=
$dot_comma      = \;
@res_in         = "in"
@res_case       = "case"
@res_of         = "of"
@res_arrow      = "->"
@res_otherwise  = "otherwise"
@reserved       = @res_let | $res_assign | $dot_comma | @res_in | @res_case | @res_of | @res_arrow | @res_otherwise

@iden           = $alpha [$alpha $digit \_ \']*
@nat_const      = $digit+


tokens :-
    @reserved               {\_ s -> ReservedWord $ case s of
                                                        "let"       -> Let
                                                        "="         -> Assignment
                                                        ";"         -> DotComma
                                                        "in"        -> In
                                                        "case"      -> Case  
                                                        "of"        -> Of
                                                        "->"        -> RightArrow
                                                        "otherwise" -> Otherwise
                            }

    "("                     { \_ _ -> BracketOpen  }
    ")"                     { \_ _ -> BracketClose }

    @nat_const              { const (NaturalConstant . read) }
    @iden                   { const Identificator }
    $white+                 ;


{

data ReservedWord = Let | Assignment | DotComma | In | Case | Of | RightArrow | Otherwise deriving(Eq, Show)

data Token = 
            ReservedWord     ReservedWord     |
            Identificator    String           |
            NaturalConstant  Int              |
            BracketOpen                       |
            BracketClose                     
            -- Code            String
                deriving (Eq, Show)

scanTokens :: String ->  [Token]
scanTokens = alexScanTokens

runScanner :: IO ()
runScanner = getContents >>= (\s -> sequence_ ( (lines s) >>= (return . print . scanTokens) ) )
}