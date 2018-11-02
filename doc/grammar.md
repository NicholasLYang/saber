# Grammar Rules

program = stmt*

stmt = let_decl | if_stmt

let_decl = "let" id "=" left_expr

left_expr = function | expression ";"

function = "\\" f_args "=>" f_body

f_body = "(" expr ")" | block

f_params = "(" ")" | "(" (param ",")* param ")"

block = "{" stmt* "}"

if_stmt = "if" comparison block opt_else

opt_else = none | "else" block

var_name = name | name "." var_name

expr = equality

equality = comparison  (("!=" | "==") comparison )*

comparison = addition ((">" | ">=" | "<" | "<=" ) addition)*

addition = multiplication (( "-" | "+" ) multiplication)*

multiplication = unary (( "/" | "*" ) unary)*

unary = ( "+" | "-" ) primary

primary = "[0-9]*(\.)?[0-9]*" | String | False | True | Nil | "(" expression ")"

# Example

let my_func = \(a, b) => (a + b)


