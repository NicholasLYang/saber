# Tokens
  
- ColonEqual	':='
- LParen	'('
- RParen	')'
- LBrace	'{'
- RBrace	'}'
- LBracket	'['
- RBracket	']'
- Comma		','
- Colon		':'
- Equal		'='
- Let		'let'
- Less		'<'
- LessEqual	'<='
- Greater	'>'
- GreaterEqual	'>='


# Grammar Rules

start = top_level_stmts*

top_level_stmts = function | stmt

function = opt_f_name f_args ":=" f_body

f_body = expr | block

opt_f_name = none | name

f_args = LParen (arg Comma)* arg RParen

block = stmt*

stmt = var_decl | if_stmt

if_stmt = If comparison LBrace block RBrace

var_decl = Let var_name Equal expr Colon

var_name = name | name Dot var_name

expr = equality

equality = comparison  ((BangEqual | EqualEqual) comparison )*

comparison = addition ((Greater | GreaterEqual | Less | LessEqual ) addition)*

addition = multiplication (( Minus | Plus ) multiplication)*

multiplication = unary (( Slash | Star ) unary)*

unary = ( Bang | Minus ) unary | primary

primary = Number | String | False | True | Nil | LParen expression RParen

# Example

my_func(a, b) := a + b

my_func(a, b) := {
  a * 5 + b	   
}

