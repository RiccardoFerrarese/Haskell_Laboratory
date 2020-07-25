-- ritornare la classe del numero (letto in stringa), e caratteri nella rappresentazione
-- intero, frazionario, floating 

{
module Main (main) where
}

%wrapper "posn"

-- macro 

$digit          = [0-9]
$exp            = [Ee]
$signpos        = \+
$signneg        = \- 
$num_comma      = \.

@unsignedInt = $digit+                                  -- num senza segno 
@signedInt   = ($signpos | $signneg ) @unsignedInt              -- num con sengo 

@int         = (@unsignedInt | @signedInt)
@fractional  = (@int $num_comma @unsignedInt?) | (@int? $num_comma @unsignedInt)           -- digit con o senza segno  comma ed eventuale altro numero
@expPart     = $exp @signedInt
@float       = ( @int | @fractional ) @expPart


-- definiamo i token utilizzati 

tokens :-
    @int                        {const readAnIntegerToken}
    @expPart                    {\_ s -> readAFloatToken s  }    
    @fractional                 {const readAFractionalToken}
    @float                      {const readAFloatToken}
    ($white | $num_comma)+          ;    


{

data Token =
        TokenInt             Integer    |
        TokenFractional      Double     |
        TokenFloat           Double     
        deriving(Show)



readAnIntegerToken :: String -> Token
readAnIntegerToken = TokenInt . read

readAFractionalToken :: String -> Token
readAFractionalToken = TokenFractional . read

readAFloatToken :: String -> Token
readAFloatToken = TokenFloat . read

scanTokens :: String -> [Token]
scanTokens = alexScanTokens 

runScanner :: IO ()
runScanner = getContents >>= (\s -> sequence_ ( (lines s) >>= (return . print . scanTokens) ) )

main = runScanner

}