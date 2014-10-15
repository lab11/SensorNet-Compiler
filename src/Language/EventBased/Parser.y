{ 
module Language.EventBased.Parser 
(
) where


import Language.EventBased.Lexer as L 
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

%%

Exp : 'DAYS'                  { 1 } 

{

parseError :: [L.Token] -> a
parseError _ = error "Parse error" 

}
