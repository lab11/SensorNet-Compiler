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

$digit    = [0-9]      		-- Digits
$octdig   = [0-7]
$hexdig   = [0-9A-Fa-f]
$alpha    = [a-zA-Z]		    -- alphabetic characters
$lower    = [a-z]
$upper    = [A-Z]
$special  = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$alphanum = [$alpha $digit]
$emailid  = [$alpha $digit \. \- \+]
$identifierchars = [$alpha $digit \_ \- \']

@escape     = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+ ) 
@char       = ($printable # $special) | @escape
@integer    = [\+\-]? $digit+
@float      = [\+\-]? $digit+ \. $digit+ ([Ee] [\+ \-]? $digit+)? 
@email      = $emailid+ \@ $alphanum+ (\. $alphanum+)+
@identifier = $lower $identifierchars*
@string     = \" ($printable # \" | @escape)* \"  -- " -- This comment keeps the
                                                       -- syntax hilighter happy

tokens :-

  -- Strucutral Elements

  $white+				              ;
  "--".*				;
  
  -- Keywords 

  "ON"                        { \s -> (Key On) } 
  "EVERY"                     { \s -> (Key Every) }
  "AFTER"                     { \s -> (Key After) }
  "BEGINS"                    { \s -> (Key Begins) }
  "END"                       { \s -> (Key End) }
  "PERFORM"                   { \s -> (Key Perform) } 
  "WITH" $white+ "COOLDOWN"   { \s -> (Key With_Cooldown) }
  "WITHIN"                    { \s -> (Key Within) }
  "GATHER"                    { \s -> (Key Gather) }
  "SEND"                      { \s -> (Key Send) }
  "EXECUTE"                   { \s -> (Key Execute) }
  "IF"                        { \s -> (Key If) }
  "DO"                        { \s -> (Key Do) }
  "SAVE"                      { \s -> (Key Save) }
  "AS"                        { \s -> (Key As) }
  "SET" $white+ "OPTIONS"     { \s -> (Key Set_Options) }
  "UPDATE"                    { \s -> (Key Update) } 

  -- Time Keywords

  "d" ("ay" "s"?)?            { \s -> (Time Days) } -- matches : 'd' 'day' 'days'
  "h" "ou"? "r" "s"?          { \s -> (Time Hours) } 
  "m" ("in" "ute"? "s"?)?     { \s -> (Time Minutes) } 
  "s" ("ec" "ond"? "s"?)?     { \s -> (Time Seconds) }

  -- Flow Control Elements

  "("                         { \s -> (Flow OpParen) }
  ")"                         { \s -> (Flow ClParen) }
  "{"                         { \s -> (Flow OpBracket) }
  "}"                         { \s -> (Flow ClBracket) }
  "["                         { \s -> (Flow OpSqBracket) }
  "]"                         { \s -> (Flow ClSqBracket) }
  ";"                         { \s -> (Flow Semicolon) }
  ":"                         { \s -> (Flow Colon) }
  ":="                        { \s -> (Flow Assign) }

  -- Operators 

  "&"                         { \s -> (Op Logical_And) }
  "|"                         { \s -> (Op Logical_Or) }
  "^"                         { \s -> (Op Logical_Xor) }
  "!"                         { \s -> (Op Logical_Not) }
  "="                         { \s -> (Op Structural_Equality) }
  ">"                         { \s -> (Op Greater_Than) }
  ">="                        { \s -> (Op Greater_Than_Equals) }
  "<"                         { \s -> (Op Less_Than) }
  "<="                        { \s -> (Op Less_Than_Equals) }
  "<<"                        { \s -> (Op String_Append) } 
  "+"                         { \s -> (Op Add) } 
  "-"                         { \s -> (Op Subtract) }
  "*"                         { \s -> (Op Multiply) } 
  "/"                         { \s -> (Op Divide) }

  -- Literals                 

  @string                     { \s -> (Lit (Str $ (tail . init) s)) } 
  @integer                    { \s -> (Lit (Integer (read s))) } 
  @float                      { \s -> (Lit (Flt (read s))) } 
  ("t"|"T")"rue"              { \s -> (Lit (Boolean True)) } 
  ("f"|"F")"alse"             { \s -> (Lit (Boolean False)) } 
  @email                      { \s -> (Lit (Email s)) } 
  @identifier                 { \s -> (Lit (Identifier s)) }
 

{

data Literals = Str String
          	  | Integer Int 
         	    | Flt Float
         	    | Boolean Bool	
         	    | Identifier String
              | Email String
            	deriving (Eq,Show,Read)

data Operators = Logical_And
          	   | Logical_Or		
          	   | Logical_Xor
          	   | Logical_Not
          	   | Structural_Equality
          	   | Greater_Than
          	   | Greater_Than_Equals
          	   | Less_Than
          	   | Less_Than_Equals
               | String_Append
          	   | Add
          	   | Subtract
          	   | Multiply
          	   | Divide
          	   deriving (Eq,Show,Read)

data FlowControl = OpParen
            	   | ClParen
            	   | OpBracket
            	   | ClBracket
            	   | OpSqBracket
            	   | ClSqBracket
            	   | Semicolon
            	   | Colon
            	   | Assign	
	               deriving (Eq,Show,Read)

data Keyword = On 	
        	   | Every 
        	   | After
        	   | Begins
        	   | End
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

data Token = Key Keyword
           | Time TimeKeyword
           | Flow FlowControl
           | Op Operators
           | Lit Literals
           deriving (Eq,Show,Read)


{-
  Option : Proc on Timer 
  Option : Proc on Interrupt 
-} 

tokenize :: String -> [Token]
tokenize = alexScanTokens

}
