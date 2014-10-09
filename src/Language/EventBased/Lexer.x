{
module Language.EventBased.Lexer 
  (
    tokenize,
    Keyword(..),
    TimeKeyword(..),
    FlowControl(..),
    Operators(..),
    Literals(..),
    Token(..)
  ) where

}

%wrapper "basic"

{- Basic Macros for Charsets -}
$digit = 0-9      		-- Digits
$alpha = [a-zA-Z]		-- alphabetic characters
$lower = [a-z]
$upper = [A-Z]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]

{- Regex Macros -}
@integer = [+-]? $digit+
@float = [+-]? $digit+ \. $digit+

tokens :-

  $white+				;
  "--".*				;
  let					{ \s -> Let }
  in					{ \s -> In }
  $digit+				{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]			{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{

data Keyword = On 	
        	   | Every 
        	   | After
        	   | Begins_While_Every
        	   | Ends_While_Every
        	   | Perform
        	   | With_Cooldown
        	   | Within
        	   | Gather 
        	   | Send
        	   | Execute 
        	   | If
        	   | Do 
        	   | Save
        	   | As
        	   | Set_Options
             | Update
        	   deriving(Eq,Show,Read)

data TimeKeyword = Days
            	   | Hours
            	   | Minutes
            	   | Seconds
            	   deriving(Eq,Show,Read)


data FlowControl = OpParen
            	   | ClParen
            	   | OpBracket
            	   | ClBracket
            	   | Semicolon
            	   | Colon
            	   | Equals	
            	   | Assign	
	               deriving (Eq,Show,Read)

data Operators = Logical_And
          	   | Logical_Or		
          	   | Logical_Xor
          	   | Logical_Not
          	   | Equals
          	   | Greater_Than
          	   | Greater_Than_Equals
          	   | Less_Than
          	   | Less_Than_Equals
          	   | Add
          	   | Subtract
          	   | Multiply
          	   | Divide
          	   deriving (Eq,Show,Read)

data Literals = Str String
          	  | Integer Int 
         	    | Flt Float
         	    | Boolean Bool	
         	    | Identifier String 
            	deriving (Eq,Show,Read)

data Token = Key Keyword
           | Time TimeKeyword
           | Flow FlowControl
           | Op Operator
           | Lit Literal
           deriving (Eq,Show,Read)


{-
  Option : Proc on Timer 
  Option : Proc on Interrupt 
-} 

tokenize :: String -> [Token]
tokenize = alexScanTokens
}
