{
module Language.EventBased.Lexer (tokenize,Token(..)) where

}

%wrapper "basic"

$digit = 0-9      -- Digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					{ \s -> Let }
  in					{ \s -> In }
  $digit+				{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]			{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
-- Each action has type :: String -> Token


data Token =
	Let 		|
	In  		|
	Sym Char	|
	Var String	|
	Int Int
	deriving (Eq,Show,Read)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}
