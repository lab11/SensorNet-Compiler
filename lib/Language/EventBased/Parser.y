{
{-# LANGUAGE TemplateHaskell #-}

module Language.EventBased.Parser 
(
  VExpr(..),
  EExpr(..),
  Interval(..),
  BinOp(..),
  UnOp(..),
  AExpr(..),
  Record(..),
  Email(..),
  Extern(..),
  ID(..),
  Rule(..),
  AAssign(..),
  Program(..),
  rules,
  valAssign,
  actAssign,
  progParse,
  valParse,
  actParse,
  evtParse,
  intParse,
  addActAssign,
  liftActAssign,
  addValAssign, 
  liftValAssign, 
  addRule,
  liftRule
) where

import Text.Show.Pretty
import qualified Language.EventBased.Lexer as L
import Data.Time (ParseTime)
import Data.Time.Clock (DiffTime,secondsToDiffTime)
import Data.Time.Format (parseTimeOrError,defaultTimeLocale)
import Data.Time.LocalTime (LocalTime,TimeOfDay)
import Control.Lens

}

%name valParse Vexpr
%name actParse Aexpr
%name evtParse Eexpr
%name intParse Interval
%name progParse Program
%tokentype{ L.Token }
%error{ parseError }

%token

  -- Operators 

  'ON'                        {L.Tok (_,L.Key L.On)} 
  'EVERY'                     {L.Tok (_,L.Key L.Every)}
  'AFTER'                     {L.Tok (_,L.Key L.After)}
  'AND'                       {L.Tok (_,L.Key L.And)}
  'WHEN'                      {L.Tok (_,L.Key L.When)}
  'FOR'                       {L.Tok (_,L.Key L.For)}
  'BECOMES FALSE'             {L.Tok (_,L.Key L.Becomes_False)}
  'BECOMES TRUE'              {L.Tok (_,L.Key L.Becomes_True)}
  'PERFORM'                   {L.Tok (_,L.Key L.Perform)} 
  'WAIT'                      {L.Tok (_,L.Key L.Wait)}
  'WITH COOLDOWN'             {L.Tok (_,L.Key L.With_Cooldown)}
  'WITHIN'                    {L.Tok (_,L.Key L.Within)}
  'GATHER'                    {L.Tok (_,L.Key L.Gather)}
  'SEND'                      {L.Tok (_,L.Key L.Send)}
  'INTO'                      {L.Tok (_,L.Key L.Into)}
  'EXECUTE'                   {L.Tok (_,L.Key L.Execute)}
  'INTERRUPT'                 {L.Tok (_,L.Key L.Interrupt)}
  'IF'                        {L.Tok (_,L.Key L.If)}
  'ELSE'                      {L.Tok (_,L.Key L.Else)}
  'DO'                        {L.Tok (_,L.Key L.Do)}
  'SAVE'                      {L.Tok (_,L.Key L.Save)}
  'AS'                        {L.Tok (_,L.Key L.As)}
  'SET OPTIONS'               {L.Tok (_,L.Key L.Set_Options)}
  'STARTING AT'               {L.Tok (_,L.Key L.Starting_At)}
  'THEN'                      {L.Tok (_,L.Key L.Then)}
  'CHECK EVERY'               {L.Tok (_,L.Key L.Check_Every)}
  'UPDATE'                    {L.Tok (_,L.Key L.Update)} 

  -- Time Keywords

  'DAYS'                      {L.Tok (_,L.RelTime L.Days)}
  'HOURS'                     {L.Tok (_,L.RelTime L.Hours)}
  'MINS'                      {L.Tok (_,L.RelTime L.Minutes)}
  'SECS'                      {L.Tok (_,L.RelTime L.Seconds)}

  -- Flow Control 

  '('                         {L.Tok (_,L.Flow L.OpParen)}
  ')'                         {L.Tok (_,L.Flow L.ClParen)}
  '{'                         {L.Tok (_,L.Flow L.OpBracket)}
  '}'                         {L.Tok (_,L.Flow L.ClBracket)}
  '['                         {L.Tok (_,L.Flow L.OpSqBracket)}
  ']'                         {L.Tok (_,L.Flow L.ClSqBracket)}
  ','                         {L.Tok (_,L.Flow L.Comma)}
  ';'                         {L.Tok (_,L.Flow L.Semicolon)}
  ':'                         {L.Tok (_,L.Flow L.Colon)}
  ':='                        {L.Tok (_,L.Flow L.Define)}
  '<-'                        {L.Tok (_,L.Flow L.Assign)}


  -- Operators 

  '&&'                        {L.Tok (_,L.Op L.Logical_And)}
  '||'                        {L.Tok (_,L.Op L.Logical_Or)}
  '^'                         {L.Tok (_,L.Op L.Logical_Xor)}
  '!'                         {L.Tok (_,L.Op L.Logical_Not)}
  '=='                        {L.Tok (_,L.Op L.Structural_Equality)}
  '>'                         {L.Tok (_,L.Op L.Greater_Than)}
  '>='                        {L.Tok (_,L.Op L.Greater_Than_Equals)}
  '<'                         {L.Tok (_,L.Op L.Less_Than)}
  '<='                        {L.Tok (_,L.Op L.Less_Than_Equals)}
  '<<'                        {L.Tok (_,L.Op L.String_Append)} 
  '+'                         {L.Tok (_,L.Op L.Add)} 
  '-'                         {L.Tok (_,L.Op L.Subtract)}
  '*'                         {L.Tok (_,L.Op L.Multiply)} 
  '/'                         {L.Tok (_,L.Op L.Divide)}

  -- Literals 

  str                         {L.Tok (_,L.Lit (L.Str $$))}
  int                         {L.Tok (_,L.Lit (L.Integer $$))}
  flt                         {L.Tok (_,L.Lit (L.Flt $$))}
  bool                        {L.Tok (_,L.Lit (L.Boolean $$))}
  email                       {L.Tok (_,L.Lit (L.Email $$))}
  id                          {L.Tok (_,L.Lit (L.Identifier $$))}
  timeOfDay                   {L.Tok (_,L.Lit (L.DailyTime $$))}  
  absTime                     {L.Tok (_,L.Lit (L.AbsTime $$))}
  extern                      {L.Tok (_,L.Lit (L.Extern $$))}   
  call                        {L.Tok (_,L.Lit (L.CallOpen $$))} -- Remember this comes with
                                                      -- an attached '('

%nonassoc '>' '<' '<=' '>=' '=='
%left '+' '-' '||' '<<'
%left '*' '/' '&&'
%right '!'
%%

EventBased : Vexpr            { $1 }

Vexpr : '(' Vexpr ')'         { $2 }
      | Vexpr '&&' Vexpr      { VEBinop Logical_And $1 $3}
      | Vexpr '||' Vexpr      { VEBinop Logical_Or $1 $3}
      | Vexpr '^' Vexpr       { VEBinop Logical_Xor $1 $3}
      | Vexpr '==' Vexpr      { VEBinop Structural_Equality $1 $3}
      | Vexpr '>' Vexpr       { VEBinop Greater_Than $1 $3}
      | Vexpr '>=' Vexpr      { VEBinop Greater_Than_Equals $1 $3}
      | Vexpr '<' Vexpr       { VEBinop Less_Than $1 $3}
      | Vexpr '<=' Vexpr      { VEBinop Less_Than_Equals $1 $3}
      | Vexpr '<<' Vexpr      { VEBinop String_Append $1 $3} 
      | Vexpr '+' Vexpr       { VEBinop Add $1 $3} 
      | Vexpr '-' Vexpr       { VEBinop Subtract $1 $3}
      | Vexpr '*' Vexpr       { VEBinop Multiply $1 $3} 
      | Vexpr '/' Vexpr       { VEBinop Divide $1 $3}
      | '!' Vexpr             { VEUnop Logical_Not $2 }
      | str                   { VEStr $1 }
      | int                   { VEInt $1 }
      | flt                   { VEFlt $1 }
      | bool                  { VEBool $1 }
      | id                    { VEId $1 }
      | Callexpr              { $1 }

Callexpr : call Params ')'    { VECall $1 $2 }

Params : Params ',' Vexpr     { $1 ++ [$3] }
       | Vexpr                { [$1] }
       | {- empty -}          { [] }

Aexprs : {- empty -}                              { [] }
       | Aexprs Aexpr                             { $1 ++ [$2] } 

Aexpr : 'GATHER' '{' Records '}' 'INTO' extern ';'{ AEGather $3 $6 } 
      | 'SEND' email Vexpr ';'                    { AESend (Email $2) $3 }
      | 'EXECUTE' Callexpr ';'                    { AEExec $2 }
      | 'IF' '(' Vexpr ')' Block ';'              { AEIf $3 $5 [] }
      | 'IF' '(' Vexpr ')' Block 'ELSE' Block ';' { AEIf $3 $5 $7 }
      | 'DO' id ';'                               { AEDo (ID $2) }
      | Vassign                                   { $1 }
   
Records : Records ',' Record                      { $1 ++ [$3] }
        | Record                                  { [$1] }

Record : 'SAVE' Vexpr 'AS' id                     { Record $2 $4}

Vassign : id '<-' Vexpr ';'                       { AEVassign (ID $1) $3 }

Aassign : id ':=' Aexpr                           { AAssign (ID $1) [$3] }
        | id ':=' Block                           { AAssign (ID $1) $3 }

Block : '{' Aexprs '}'                            { $2 } 

Interval : SubIntervals                           { Interval (sum $1) }

Schedule : 'WHEN' '(' Eexpr ')' 'FOR' RBlock      {} -- TODO

RBlock : '{' Rules '}'                            {} -- TODO

Rules : Rule Rules                                {} -- TODO
      | Rule                                      {}

SubIntervals : SubIntervals SubInterval           { $2 : $1 }
             | SubInterval                        { [$1] }

SubInterval : int 'SECS'                          { $1 }
            | int 'MINS'                          { $1 * 60 }
            | int 'HOURS'                         { $1 * 60 * 60 } 
            | int 'DAYS'                          { $1 * 60 * 60 * 24 }

Eexpr : 'EVERY' Interval                          { EVEvery $2 }
      | 'EVERY' Interval 'STARTING AT' absTime    { EVStartingAt $2 $4 }
      | Interval 'AFTER' Eexpr                    { EVAfter $1 $3 }
      | 'INTERRUPT' extern                        { EVInterrupt (Extern $2) }
      | Eexpr 'WITH COOLDOWN' Interval            { EVCooldown $1 $3 }
      | 'CHECK EVERY' Interval 'AND' 'WHEN' Vexpr 'BECOMES TRUE' 
                                                  { EVBegins $5 $2 }
      | 'CHECK EVERY' Interval 'AND' 'WHEN' Vexpr 'BECOMES FALSE' 
                                                  { EVEnds $5 $2 }
      | Eexpr 'WAIT' Interval 'THEN'              { EVAfter $3 $1}
      | '(' Eexpr ')'                             { $2 }


Rule : 'ON' '(' Eexpr ')' Block                   { Rule $3 $5 }

Program : Vassign Program                         { addValAssign $2 $1 }
        | Aassign Program                         { addActAssign $2 $1 }
        | Rule Program                            { addRule $2 $1 }
        | Schedule Program                        { $2 } -- TODO
        | {- empty -}                             { Program [] [] [] }

{

newtype ID = ID String 
           deriving (Show,Read,Eq,Ord)

newtype Email = Email String
              deriving (Show,Read,Eq,Ord)

newtype Extern = Extern String
               deriving (Show,Read,Eq,Ord)

data AAssign = AAssign ID [AExpr]
             deriving (Show,Read,Eq,Ord)

data Rule = Rule EExpr [AExpr]
          deriving (Show,Read,Eq,Ord)
             
data EExpr = EVEvery Interval
           | EVStartingAt Interval LocalTime
           | EVAfter Interval EExpr
           | EVInterrupt Extern
           | EVCooldown EExpr Interval
           | EVBegins VExpr Interval 
           | EVEnds VExpr Interval 
           deriving (Show,Read,Eq,Ord)

data AExpr = AEGather [Record] String 
           | AESend Email VExpr
           | AEExec VExpr
           | AEIf VExpr [AExpr] [AExpr]
           | AEDo ID
           | AEVassign ID VExpr 
           deriving (Show,Read,Eq,Ord)

data Record = Record VExpr String
            deriving (Show,Read,Eq,Ord)

data VExpr = VEBinop BinOp VExpr VExpr
           | VEUnop UnOp VExpr
           | VEStr String
           | VEInt Int
           | VEFlt Float
           | VEBool Bool
           | VEId String
           | VECall String [VExpr]
           deriving (Show,Read,Eq,Ord)

data BinOp = Logical_And
       	   | Logical_Or		
       	   | Logical_Xor
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
           deriving (Show,Read,Eq,Ord)

data UnOp = Logical_Not
          deriving (Show,Read,Eq,Ord)

newtype Interval = Interval Int -- Units are seconds 
                 deriving (Show,Read,Eq,Ord)

-- The AST type
data Program = Program {
  _actAssign :: [AAssign],
  _valAssign :: [AExpr],
  _rules :: [Rule]
} deriving (Show,Read,Eq,Ord)

-- ActAssignment Lenses and Utility Function

addActAssign :: Program -> AAssign -> Program 
addActAssign p a = liftActAssign ([a] ++) p 

liftActAssign :: ([AAssign] -> [AAssign]) -> Program -> Program 
liftActAssign = over actAssign

actAssign :: Lens Program Program [AAssign] [AAssign]
actAssign = lens getActAssign setActAssign

getActAssign :: Program -> [AAssign]
getActAssign = _actAssign

setActAssign :: Program -> [AAssign] -> Program 
setActAssign (Program _ v r) na = Program na v r 

-- ValAssignment Lenses and Utility Function

addValAssign :: Program -> AExpr -> Program 
addValAssign p v = liftValAssign ([v] ++) p 

liftValAssign :: ([AExpr] -> [AExpr]) -> Program -> Program 
liftValAssign = over valAssign

valAssign :: Lens Program Program [AExpr] [AExpr]
valAssign = lens getValAssign setValAssign

getValAssign :: Program -> [AExpr]
getValAssign = _valAssign

setValAssign :: Program -> [AExpr] -> Program 
setValAssign (Program a _ r) nv = Program a nv r 

-- Rule Lenses and Utility Function

addRule :: Program -> Rule -> Program 
addRule p r = liftRule ([r] ++) p 

liftRule :: ([Rule] -> [Rule]) -> Program -> Program
liftRule = over rules

rules :: Lens Program Program [Rule] [Rule]
rules = lens getRules setRules

getRules :: Program -> [Rule]
getRules = _rules

setRules :: Program -> [Rule] -> Program 
setRules (Program a v _) nr = Program a v nr 

-- Error Handling  

parseError :: [L.Token] -> a
parseError ((L.Tok (a,t)):_) = error ("Parse error before : " ++ (ppShow t) ++
                           "\nAt location : " ++ (ppShow a) )
}
