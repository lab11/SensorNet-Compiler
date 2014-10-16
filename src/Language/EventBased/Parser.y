{ 
module Language.EventBased.Parser 
(
  VExpr(..),
  BinOp(..),
  UnOp(..),
) where

import qualified Language.EventBased.Lexer as L 

}

%name eventBased
%tokentype{ L.Token }
%error{ parseError }

%token

  -- Operators 

  'ON'                        {L.Key L.On} 
  'EVERY'                     {L.Key L.Every}
  'AFTER'                     {L.Key L.After}
  'BEGINS'                    {L.Key L.Begins}
  'END'                       {L.Key L.End}
  'PERFORM'                   {L.Key L.Perform} 
  'WITH COOLDOWN'             {L.Key L.With_Cooldown}
  'WITHIN'                    {L.Key L.Within}
  'GATHER'                    {L.Key L.Gather}
  'SEND'                      {L.Key L.Send}
  'EXECUTE'                   {L.Key L.Execute}
  'IF'                        {L.Key L.If}
  'DO'                        {L.Key L.Do}
  'SAVE'                      {L.Key L.Save}
  'AS'                        {L.Key L.As}
  'SET OPTIONS'               {L.Key L.Set_Options}
  'UPDATE'                    {L.Key L.Update} 

  -- Time Keywords

  'DAYS'                      {L.Time L.Days}
  'HOURS'                     {L.Time L.Hours}
  'MINS'                      {L.Time L.Minutes}
  'SECS'                      {L.Time L.Seconds}

  -- Flow Control 

  '('                         {L.Flow L.OpParen}
  ')'                         {L.Flow L.ClParen}
  '{'                         {L.Flow L.OpBracket}
  '}'                         {L.Flow L.ClBracket}
  '['                         {L.Flow L.OpSqBracket}
  ']'                         {L.Flow L.ClSqBracket}
  ','                         {L.Flow L.Comma}
  ';'                         {L.Flow L.Semicolon}
  ':'                         {L.Flow L.Colon}
  ':='                        {L.Flow L.Assign}

  -- Operators 

  '&&'                        {L.Op L.Logical_And}
  '||'                        {L.Op L.Logical_Or}
  '^'                         {L.Op L.Logical_Xor}
  '!'                         {L.Op L.Logical_Not}
  '=='                        {L.Op L.Structural_Equality}
  '>'                         {L.Op L.Greater_Than}
  '>='                        {L.Op L.Greater_Than_Equals}
  '<'                         {L.Op L.Less_Than}
  '<='                        {L.Op L.Less_Than_Equals}
  '<<'                        {L.Op L.String_Append} 
  '+'                         {L.Op L.Add} 
  '-'                         {L.Op L.Subtract}
  '*'                         {L.Op L.Multiply} 
  '/'                         {L.Op L.Divide}

  -- Literals 

  str                         {L.Lit (L.Str $$)}
  int                         {L.Lit (L.Integer $$)}
  flt                         {L.Lit (L.Flt $$)}
  bool                        {L.Lit (L.Boolean $$)}
  email                       {L.Lit (L.Email $$)}
  id                          {L.Lit (L.Identifier $$)}
  call                        {L.Lit (L.CallOpen $$)} -- Remember this comes with
                                                      --  an attached '('

%%

-- eventBased : vexpr            { $1 }

binop : '&&'                  {Logical_And}
      | '||'                  {Logical_Or}
      | '^'                   {Logical_Xor}
      | '=='                  {Structural_Equality}
      | '>'                   {Greater_Than}
      | '>='                  {Greater_Than_Equals}
      | '<'                   {Less_Than}
      | '<='                  {Less_Than_Equals}
      | '<<'                  {String_Append} 
      | '+'                   {Add} 
      | '-'                   {Subtract}
      | '*'                   {Multiply} 
      | '/'                   {Divide}


unop : '!'                    {Logical_Not}

vexpr : '(' vexpr ')'         { $2 }
      | vexpr binop vexpr     { VEBinop $2 $1 $3 }
      | unop vexpr            { VEUnop $1 $2 }
      | str                   { VEStr $1 }
      | int                   { VEInt $1 }
      | flt                   { VEFlt $1 }
      | bool                  { VEBool $1 }
      | email                 { VEEmail $1 }
      | id                    { VEId $1 }
      | call params ')'       { VECall $1 $2 }

params : params ',' vexpr     { $1 ++ [$3] }
       | vexpr                { [$1] }

{
{-

newtype ID = ID String 
           deriving (Show,Read,Eq,Ord)

newtype Rule = Rule Event [Action]  
             deriving (Show,Read,Eq,Ord)

data Assignable = AsblVal VExpr
                | AsblAct [Action]
                deriving (Show,Read,Eq,Ord)

type Assignment = (ID,Assignable)

-}

data VExpr = VEBinop BinOp VExpr VExpr
           | VEUnop UnOp VExpr
           | VEStr String
           | VEInt Int
           | VEFlt Float
           | VEBool Bool
           | VEEmail String
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

{-
-- The AST type
data Program = Program {
  assigned :: [Assignment],
  rules :: [Rule]
} deriving (Show,Read,Eq,Ord)

addAssign :: Program -> Assignment -> Program 
addAssign p a =  p{assigned=na}
  where na = a : assigned p

addRule :: Program -> Rule -> Program 
addRule p r =  p{rules=nr}
  where nr = r : rules p

-}

parseError :: [L.Token] -> a
parseError _ = error "Parse error" 


}
