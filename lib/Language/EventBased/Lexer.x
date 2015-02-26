{
module Language.EventBased.Lexer 
  (
    tokenize,
    Keyword(..),
    TimeKeyword(..),
    FlowControl(..),
    Operators(..),
    Literals(..),
    IToken(..),
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

  "ON"                        { \ a s -> ins a (Key On) } 
  "EVERY"                     { \ a s -> ins a (Key Every) }
  "AFTER"                     { \ a s -> ins a (Key After) }
  "AND"                       { \ a s -> ins a (Key And) }
  "BECOMES" $white+ "FALSE"   { \ a s -> ins a (Key Becomes_False) }
  "BECOMES" $white+ "TRUE"    { \ a s -> ins a (Key Becomes_True) }
  "PERFORM"                   { \ a s -> ins a (Key Perform) } 
  "WAIT"                      { \ a s -> ins a (Key Wait) }
  "WHEN"                      { \ a s -> ins a (Key When) }
  "FOR"                       { \ a s -> ins a (Key For) }
  "WITH" $white+ "COOLDOWN"   { \ a s -> ins a (Key With_Cooldown) }
  "WITHIN"                    { \ a s -> ins a (Key Within) }
  "INTERRUPT"                 { \ a s -> ins a (Key Interrupt) }
  "GATHER"                    { \ a s -> ins a (Key Gather) }
  "SEND"                      { \ a s -> ins a (Key Send) }
  "EXECUTE"                   { \ a s -> ins a (Key Execute) }
  "IF"                        { \ a s -> ins a (Key If) }
  "ELSE"                      { \ a s -> ins a (Key Else) }
  "DO"                        { \ a s -> ins a (Key Do) }
  "SAVE"                      { \ a s -> ins a (Key Save) }
  "INTO"                      { \ a s -> ins a (Key Into) }
  "AS"                        { \ a s -> ins a (Key As) }
  "SET" $white+ "OPTIONS"     { \ a s -> ins a (Key Set_Options) }
  "CHECK" $white+ "EVERY"     { \ a s -> ins a (Key Check_Every) }
  "STARTING" $white+ "AT"     { \ a s -> ins a (Key Starting_At) }
  "THEN"                      { \ a s -> ins a (Key Then) }
  "UPDATE"                    { \ a s -> ins a (Key Update) } 

  -- Time Keywords

  "d" ("ay" "s"?)?            { \ a s -> ins a (RelTime Days) } 
  "h" ("ou"? "r" "s"?)?       { \ a s -> ins a (RelTime Hours) } 
  "m" ("in" "ute"? "s"?)?     { \ a s -> ins a (RelTime Minutes) } 
  "s" ("ec" "ond"? "s"?)?     { \ a s -> ins a (RelTime Seconds) }

  -- Flow Control Elements

  "("                         { \ a s -> ins a (Flow OpParen) }
  ")"                         { \ a s -> ins a (Flow ClParen) }
  "{"                         { \ a s -> ins a (Flow OpBracket) }
  "}"                         { \ a s -> ins a (Flow ClBracket) }
  "["                         { \ a s -> ins a (Flow OpSqBracket) }
  "]"                         { \ a s -> ins a (Flow ClSqBracket) }
  ";"                         { \ a s -> ins a (Flow Semicolon) }
  ":"                         { \ a s -> ins a (Flow Colon) }
  ","                         { \ a s -> ins a (Flow Comma) }
  ":="                        { \ a s -> ins a (Flow Define) }
  "<-"                        { \ a s -> ins a (Flow Assign) }

  -- Operators 

  "&&"                        { \ a s -> ins a (Op Logical_And) }
  "||"                        { \ a s -> ins a (Op Logical_Or) }
  "^"                         { \ a s -> ins a (Op Logical_Xor) }
  "!"                         { \ a s -> ins a (Op Logical_Not) }
  "=="                        { \ a s -> ins a (Op Structural_Equality) }
  ">"                         { \ a s -> ins a (Op Greater_Than) }
  ">="                        { \ a s -> ins a (Op Greater_Than_Equals) }
  "<"                         { \ a s -> ins a (Op Less_Than) }
  "<="                        { \ a s -> ins a (Op Less_Than_Equals) }
  "<<"                        { \ a s -> ins a (Op String_Append) } 
  "+"                         { \ a s -> ins a (Op Add) } 
  "-"                         { \ a s -> ins a (Op Subtract) }
  "*"                         { \ a s -> ins a (Op Multiply) } 
  "/"                         { \ a s -> ins a (Op Divide) }

  -- Literals                 

  @string                     { \ a s -> ins a (Lit (Str $ read s)) } 
  @integer                    { \ a s -> ins a (Lit (Integer $ read s )) } 
  @float                      { \ a s -> ins a (Lit (Flt $ read s))} 
  ("t"|"T")"rue"              { \ a s -> ins a (Lit (Boolean True))} 
  ("f"|"F")"alse"             { \ a s -> ins a (Lit (Boolean False))} 
  @email                      { \ a s -> ins a (Lit (Email s)) } 
  @identifier                 { \ a s -> ins a (Lit (Identifier s)) }
  @opencall                   { \ a s -> ins a (Lit (CallOpen $ init s)) }
  @external                   { \ a s -> ins a (Lit (Extern s)) }

  -- Absolute Time Literals 

  @date                       { \ a s -> ins a (Lit (AbsTime (parseTime "%m/%d/%Y" s)))}
  @date $white+ @time12       { \ a s -> ins a (Lit (AbsTime (parseTime "%m/%d/%Y %r" s)))}
  @date $white+ @time24       { \ a s -> ins a (Lit (AbsTime (parseTime "%m/%d/%Y %X" s)))}
  @time24                     { \ a s -> ins a (Lit (DailyTime (parseTime "%X" s)))}
  @time12                     { \ a s -> ins a (Lit (DailyTime (parseTime "%r" s)))}
  
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
                  | Define
            	   | Assign	
	               deriving (Eq,Show,Read)

data Keyword = On 	
        	   | Every 
        	   | After
             | And
             | Becomes_False
             | Becomes_True
        	   | Perform
             | Wait
             | When
             | For
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
             | Then
             | Check_Every
             | Update
             | Interrupt
        	   deriving(Eq,Show,Read)

data TimeKeyword = Days
            	   | Hours
            	   | Minutes
            	   | Seconds
            	   deriving(Eq,Show,Read)

data IToken = Key Keyword
           | RelTime TimeKeyword
           | Flow FlowControl
           | Op Operators
           | Lit Literals
           deriving (Eq,Show,Read)

{-
  Option : Proc on Timer 
  Option : Proc on Interrupt 
-} 

data Token = Tok (AlexPosn,IToken)
        deriving (Eq,Show)

parseTime :: ParseTime t => String -> String -> t
parseTime = parseTimeOrError True defaultTimeLocale 

tokenize :: String -> [Token]
tokenize = alexScanTokens

ins a t = Tok (a,t)
}
