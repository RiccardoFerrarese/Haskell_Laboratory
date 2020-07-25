{
module Main (main) where
}

%wrapper "basic"

-- macro 
$digit          = [0-9]
$small_let      =   a-z
$capital_let    =   A-Z
$letter         =   [$small_let $capital_let]
@blockOpen      = "{-" $white* 
@blockClose     = $white* "-}"
@dash           = "--"

@text               = $letter ($letter | $digit | $white+ )*
@inLineComment      = @dash @text
@blockComment       = @blockOpen @text @blockClose
@nastedComment      = @blockOpen @blockComment @blockClose



tokens :-
    @text               { TextToken }
    @inLineComment      {  InLineCommentToken }
    @blockComment       {  BlockCommentToken }
    @nastedComment      {  NastedCommentToken }
    $white+             ;    


{
data Token = 
        TextToken           String |
        InLineCommentToken  String |
        BlockCommentToken   String |
        NastedCommentToken  String 
        deriving( Show)


scanTokens :: String -> [Token]
scanTokens = alexScanTokens 

runScanner :: IO ()
runScanner = getContents >>= (\s -> sequence_ ( (lines s) >>= (return . print . scanTokens) ) )

main = runScanner

}