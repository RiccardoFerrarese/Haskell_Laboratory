{
    -- modulo che poi vado ad importare
module ScannerExp(runScanner, scanTokens,Token(..),BracketState(..),Operator(..)) where
}

%wrapper "posn"

-- macro 

$digit          =   0-9
$letter         =   [a-z A-Z]

$plus   =   \+
$minus  =   \-
$times  =   \*
$div    =   [ \\ \: ]   -- doppia possibilità per la divisione
@op     =   $plus | $minus | $times | $div 

$rnd_o  =   \(
$rnd_c  =   \)
$sq_o   =   \[
$sq_c   =   \]
$gr_o   =   \{
$gr_c   =   \}

@nat_const = $digit+


-- tokens 

tokens :-
    @nat_const      {\_ -> NaturalNumber . read    }

    @op             {\_ s -> Operator $ case s of
                                            "+"  -> Plus
                                            "-"  -> Minus
                                            "*"  -> Times
                                            "\\" -> Divide
                                            ":"  -> Divide
                    }    

    $rnd_o          {\_ _ -> TokenRoundBr Open      }
    $rnd_c          {\_ _ -> TokenRoundBr Close     }
    $sq_c           {\_ _ -> TokenSquareBr Close    }
    $sq_o           {\_ _ -> TokenSquareBr Open     }
    $gr_o           {\_ _ -> TokenGraphBr Open      }
    $gr_c           {\_ _ -> TokenGraphBr Close     }

    $white+         ;

{
data BracketState = Open | Close deriving (Eq, Show)
data Operator     = Plus | Minus | Times | Divide deriving (Eq, Show)

data Token =    
            NaturalNumber Int           |
            Operator      Operator      |
            TokenRoundBr  BracketState  |
            TokenSquareBr BracketState  |
            TokenGraphBr  BracketState  
                deriving(Eq, Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

runScanner :: IO ()
runScanner = getContents >>= (\s -> sequence_ $ (lines s) >>= (return . print . scanTokens) )
}