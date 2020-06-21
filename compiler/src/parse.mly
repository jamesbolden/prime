%token EOF
%token BACKTICK
%token BAR
%token DEF
%token ELLIPSES
%token RIGHT_ARROW
%token LEFT_ARROW
%token FORALL
%token TYPE
%token FAT_RIGHT_ARROW
%token BACKSLASH
%token LEFT_CURLY_BRACKET
%token RIGHT_CURLY_BRACKET
%token LEFT_SQUARE_BRACKET
%token RIGHT_SQUARE_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token NIL
%token LET
%token OF
%token TYPE_ANNOTATION
%token UNIT
%token WHERE
%token WITH
%token WILD
%token COMMA
%token IN
%token IF
%token THEN
%token ELSE
%token DO
%token WHILE
%token CASE
%token CLASS
%token INST
%token DATA
%token NEWTYPE
%token PACKAGE
%token IMPORT
%token DOT
%token INFIX
%token INFIXL
%token INFIXR
%token RULE
%token SEMICOLON
%token OBJECT
%token LAYOUT_START
%token LAYOUT_SEPARATOR
%token LAYOUT_END
%token <string> STRING
%token <string> LOWER_IDENT
%token <string> UPPER_IDENT
%token <string> OPERATOR
%token <int> INT
%token <int> PRIM_INT
%token <Uchar.t> CHAR
%token <Uchar.t> PRIM_CHAR
%token <float> FLOAT
%token <float> PRIM_FLOAT

%start <Ast.P.t> prog

%%

%public %inline opt_plist(X):
  | xs = loption(delimited(LEFT_PAREN, separated_nonempty_list(COMMA, X), RIGHT_PAREN)) { xs }

%public %inline plist(X):
  | xs = delimited(LEFT_PAREN, separated_nonempty_list(COMMA, X), RIGHT_PAREN)          { xs }

prog:
  | x = pkg_sig; xs = pkg_defn*                                                         { x, xs }

pkg_sig:
  | PACKAGE; n = UPPER_IDENT; xs = exports                                              { n, xs }
%inline exports:
  |                                                                                     { [] }
  | WITH; xs = separated_nonempty_list(COMMA, export_term)                              { xs }
%inline export_term:
  | x = LOWER_IDENT                                                                     { Ast.Export.Definition x }
  | x = UPPER_IDENT                                                                     { Ast.Export.Group_name x }
  | x = UPPER_IDENT; LEFT_PAREN; ELLIPSES; RIGHT_PAREN                                  { Ast.Export.Whole_group x }
  | x = UPPER_IDENT; xs = plist(identifier)                                             { Ast.Export.Group (x, xs) }

pkg_defn:
  | x = import_stmt                                                                     { x }

%inline import_stmt:
  | IMPORT; x = UPPER_IDENT                                                             { Ast.T.Import (Ast.Import.Whole_pkg x) }
  | IMPORT; x = UPPER_IDENT; xs = plist(import_term)                                    { Ast.T.Import (Ast.Import.From_pkg (x, xs)) }
%inline import_term:
  | x = LOWER_IDENT                                                                     { Ast.Import.Definition x }
  | x = UPPER_IDENT                                                                     { Ast.Import.Group_name x }
  | x = UPPER_IDENT; LEFT_PAREN; ELLIPSES; RIGHT_PAREN                                  { Ast.Import.Whole_group x }
  | x = UPPER_IDENT; xs = plist(identifier)                                             { Ast.Import.Group (x, xs) }

%inline identifier:
  | x = LOWER_IDENT                                                                     { x }
  | x = UPPER_IDENT                                                                     { x }