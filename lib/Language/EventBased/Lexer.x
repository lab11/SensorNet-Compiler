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

import Data.Time (ParseTime)
import Data.Time.Format (parseTimeOrError,defaultTimeLocale)
import Data.Time.LocalTime (LocalTime,TimeOfDay)
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

@month    = ('0'? [1-9]| '1' [12])
@day      = ('0'? [1-9]| [1-2][0-9]|'3'[01])
@year     = ('1' '9'|'2' [0-9])[0-9][0-9]
@date     = @month '\/' @day '\/' @year

@hour24     = (('0'?|'1')[0-9]|'2'[0-4])
@hour12     = ('0'?[0-9]|'1'[0-2])
@minute     = [0-5][0-9]
@second     = [0-5][0-9]
@dayhalf    = ('a'|'p')'m'
@time12     = @hour12 ':' @minute ':' @second $white+ @dayhalf
@time24     = @hour12 ':' @minute ':' @second 

@escape     = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+ ) 
@char       = ($printable # $special) | @escape
@integer    = [\+\-]? $digit+
@float      = [\+\-]? $digit+ \. $digit+ ([Ee] [\+ \-]? $digit+)? 
@email      = $emailid+ \@ $alphanum+ (\. $alphanum+)+
@identifier = $lower $identifierchars*
@external   = $upper $identifierchars*
@opencall   = @external \(
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
  "INTERRUPT"                 { \s -> (Key Interrupt) }
  "GATHER"                    { \s -> (Key Gather) }
  "SEND"                      { \s -> (Key Send) }
  "EXECUTE"                   { \s -> (Key Execute) }
  "IF"                        { \s -> (Key If) }
  "DO"                        { \s -> (Key Do) }
  "SAVE"                      { \s -> (Key Save) }
  "INTO"                      { \s -> (Key Into) }
  "AS"                        { \s -> (Key As) }
  "SET" $white+ "OPTIONS"     { \s -> (Key Set_Options) }
  "STARTING" $white+ "AT"     { \s -> (Key Starting_At) }
  "UPDATE"                    { \s -> (Key Update) } 

  -- Time Keywords

  "d" ("ay" "s"?)?            { \s -> (RelTime Days) } 
  "h" "ou"? "r" "s"?          { \s -> (RelTime Hours) } 
  "m" ("in" "ute"? "s"?)?     { \s -> (RelTime Minutes) } 
  "s" ("ec" "ond"? "s"?)?     { \s -> (RelTime Seconds) }

  -- Flow Control Elements

  "("                         { \s -> (Flow OpParen) }
  ")"                         { \s -> (Flow ClParen) }
  "{"                         { \s -> (Flow OpBracket) }
  "}"                         { \s -> (Flow ClBracket) }
  "["                         { \s -> (Flow OpSqBracket) }
  "]"                         { \s -> (Flow ClSqBracket) }
  ";"                         { \s -> (Flow Semicolon) }
  ":"                         { \s -> (Flow Colon) }
  ","                         { \s -> (Flow Comma) }
  ":="                        { \s -> (Flow Assign) }

  -- Operators 

  "&&"                        { \s -> (Op Logical_And) }
  "||"                        { \s -> (Op Logical_Or) }
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

  @string                     { \s -> (Lit (Str $ read s)) } 
  @integer                    { \s -> (Lit (Integer $ read s )) } 
  @float                      { \s -> (Lit (Flt $ read s))} 
  ("t"|"T")"rue"              { \s -> (Lit (Boolean True))} 
  ("f"|"F")"alse"             { \s -> (Lit (Boolean False))} 
  @email                      { \s -> (Lit (Email s)) } 
  @identifier                 { \s -> (Lit (Identifier s)) }
  @opencall                   { \s -> (Lit (CallOpen $ init s)) }
  @external                   { \s -> (Lit (Extern s)) }

  -- Absolute Time Literals 

  @date                       { \s -> (Lit (AbsTime (parseTime "%D" s)))}
  @date @time12               { \s -> (Lit (AbsTime (parseTime "%D %r" s)))}
  @date @time24               { \s -> (Lit (AbsTime (parseTime "%D %X" s)))}
  @time24                     { \s -> (Lit (DailyTime (parseTime "%X" s)))}
  @time12                     { \s -> (Lit (DailyTime (parseTime "%r" s)))}
  
{

data Literals = Str String
          	  | Integer Int 
         	    | Flt Float
         	    | Boolean Bool	
         	    | Identifier String
              | Email String
              | CallOpen String
              | Extern String
              | AbsTime LocalTime
              | DailyTime TimeOfDay 
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
                 | Comma
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
        	   | Else
        	   | Do 
        	   | Into
        	   | Save
        	   | As
        	   | Set_Options
             | Starting_At
             | Update
             | Interrupt
        	   deriving(Eq,Show,Read)

data TimeKeyword = Days
            	   | Hours
            	   | Minutes
            	   | Seconds
            	   deriving(Eq,Show,Read)

data Token = Key Keyword
           | RelTime TimeKeyword
           | Flow FlowControl
           | Op Operators
           | Lit Literals
           deriving (Eq,Show,Read)

{-
  Option : Proc on Timer 
  Option : Proc on Interrupt 
-} 

parseTime :: ParseTime t => String -> String -> t
parseTime = parseTimeOrError True defaultTimeLocale 

tokenize :: String -> [Token]
tokenize = alexScanTokens

}
