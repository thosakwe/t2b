grammar T2B;

pattern:
  ID #IdPattern
  | '_' #IgnorePattern
;

expr:
  ID #IdExpr
  | NUM #NumExpr
  | 'true' #TrueExpr
  | 'false' #FalseExpr
  | '(' expr ')' #ParenExpr
  | target=expr arg=expr #CallExpr
;

WS: [\nrt ]+ -> skip;

NUM: '-'? [0-9]+ ('.' [0-9]+)? ([Ee] '-'? [0-9]+)?;
ID: [A-Za-z][A-Za-z0-9_]*;
