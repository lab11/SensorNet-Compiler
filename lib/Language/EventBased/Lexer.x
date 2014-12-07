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

%wrapper "posn"

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

@month    = ("0"? [1-9]| "1" [0-2])
@day      = ("0"? [1-9]| [1-2][0-9] | "3" [0-1])
@year     = ("19" [0-9]{2}| "2" [0-9]{3})
@date     = @month "/" @day "/" @year

@hour24     = (("0"?|"1")[0-9]|"2"[0-3])
@hour12     = ("0"? [1-9]|"1"[0-2])
@minute     = [0-5][0-9]
@second     = [0-5][0-9]
@dayhalf    = ("a"|"p")"m"
@time12     = @hour12 ":" @minute ":" @second $white+ @dayhalf
@time24     = @hour24 ":" @minute ":" @second 

@escape     = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+ ) 
@char       = ($printable # $special) | @escape
@integer    = [\+\-]? $digit+
@float      = [\+\-]? $digit+ (\. $digit+ | (\. $digit+)? ([Ee] [\+ \-]? $digit+)?) 
@email      = $emailid+ \@ $alphanum+ (\. $alphanum+)+
@identifier = $lower $identifierchars*
@external   = $upper ($identifierchars* $lower $identifierchars*)?
@opencall   = @external \(
@string     = \" ($printable # \" | @escape)* \"  -- " -- This comment keeps the
                                                       -- syntax hilighter happy

tokens :-

  -- Strucutral Elements

  $white+				              ;
  "--".*				              ;
  
  -- Keywords 

  "ON"                        { \ _ s -> (Key On) } 
  "EVERY"                     { \ _ s -> (Key Every) }
  "AFTER"                     { \ _ s -> (Key After) }
  "BEGINS"                    { \ _ s -> (Key Begins) }
  "ENDS"                      { \ _ s -> (Key Ends) }
  "PERFORM"                   { \ _ s -> (Key Perform) } 
  "WITH" $white+ "COOLDOWN"   { \ _ s -> (Key With_Cooldown) }
  "WITHIN"                    { \ _ s -> (Key Within) }
  "INTERRUPT"                 { \ _ s -> (Key Interrupt) }
  "GATHER"                    { \ _ s -> (Key Gather) }
  "SEND"                      { \ _ s -> (Key Send) }
  "EXECUTE"                   { \ _ s -> (Key Execute) }
  "IF"                        { \ _ s -> (Key If) }
  "ELSE"                      { \ _ s -> (Key Else) }
  "DO"                        { \ _ s -> (Key Do) }
  "SAVE"                      { \ _ s -> (Key Save) }
  "INTO"                      { \ _ s -> (Key Into) }
  "AS"                        { \ _ s -> (Key As) }
  "SET" $white+ "OPTIONS"     { \ _ s -> (Key Set_Options) }
  "CHECKING" $white+ "EVERY"  { \ _ s -> (Key Checking_Every) }
  "STARTING" $white+ "AT"     { \ _ s -> (Key Starting_At) }
  "UPDATE"                    { \ _ s -> (Key Update) } 

  -- Time Keywords

  "d" ("ay" "s"?)?            { \ _ s -> (RelTime Days) } 
  "h" ("ou"? "r" "s"?)?       { \ _ s -> (RelTime Hours) } 
  "m" ("in" "ute"? "s"?)?     { \ _ s -> (RelTime Minutes) } 
  "s" ("ec" "ond"? "s"?)?     { \ _ s -> (RelTime Seconds) }

  -- Flow Control Elements

  "("                         { \ _ s -> (Flow OpParen) }
  ")"                         { \ _ s -> (Flow ClParen) }
  "{"                         { \ _ s -> (Flow OpBracket) }
  "}"                         { \ _ s -> (Flow ClBracket) }
  "["                         { \ _ s -> (Flow OpSqBracket) }
  "]"                         { \ _ s -> (Flow ClSqBracket) }
  ";"                         { \ _ s -> (Flow Semicolon) }
  ":"                         { \ _ s -> (Flow Colon) }
  ","                         { \ _ s -> (Flow Comma) }
  ":="                        { \ _ s -> (Flow Assign) }

  -- Operators 

  "&&"                        { \ _ s -> (Op Logical_And) }
  "||"                        { \ _ s -> (Op Logical_Or) }
  "^"                         { \ _ s -> (Op Logical_Xor) }
  "!"                         { \ _ s -> (Op Logical_Not) }
  "=="                        { \ _ s -> (Op Structural_Equality) }
  ">"                         { \ _ s -> (Op Greater_Than) }
  ">="                        { \ _ s -> (Op Greater_Than_Equals) }
  "<"                         { \ _ s -> (Op Less_Than) }
  "<="                        { \ _ s -> (Op Less_Than_Equals) }
  "<<"                        { \ _ s -> (Op String_Append) } 
  "+"                         { \ _ s -> (Op Add) } 
  "-"                         { \ _ s -> (Op Subtract) }
  "*"                         { \ _ s -> (Op Multiply) } 
  "/"                         { \ _ s -> (Op Divide) }

  -- Literals                 

  @string                     { \ _ s -> (Lit (Str $ read s)) } 
  @integer                    { \ _ s -> (Lit (Integer $ read s )) } 
  @float                      { \ _ s -> (Lit (Flt $ read s))} 
  ("t"|"T")"rue"              { \ _ s -> (Lit (Boolean True))} 
  ("f"|"F")"alse"             { \ _ s -> (Lit (Boolean False))} 
  @email                      { \ _ s -> (Lit (Email s)) } 
  @identifier                 { \ _ s -> (Lit (Identifier s)) }
  @opencall                   { \ _ s -> (Lit (CallOpen $ init s)) }
  @external                   { \ _ s -> (Lit (Extern s)) }

  -- Absolute Time Literals 

  @date                       { \ _ s -> (Lit (AbsTime (parseTime "%m/%d/%Y" s)))}
  @date $white+ @time12       { \ _ s -> (Lit (AbsTime (parseTime "%m/%d/%Y %r" s)))}
  @date $white+ @time24       { \ _ s -> (Lit (AbsTime (parseTime "%m/%d/%Y %X" s)))}
  @time24                     { \ _ s -> (Lit (DailyTime (parseTime "%X" s)))}
  @time12                     { \ _ s -> (Lit (DailyTime (parseTime "%r" s)))}
  
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
        	   | Ends
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
             | Checking_Every
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
