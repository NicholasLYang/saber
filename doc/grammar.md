# Grammar Rules

program = stmt*

stmt = "let" id "=" left_expr

left_expr = function | expression ";"

function = "\\" f_args "=>" f_body

f_body = "(" expr ")" | block

f_args = "(" (arg ",")* arg? ")"

block = "{" stmt* "}"

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

let my_func = \(a, b) => (a + b)


