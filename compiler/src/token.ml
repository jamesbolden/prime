type token =
    | EOF
    | BACKTICK
    | BAR
    | DEF
    | ELLIPSES
    | RIGHT_ARROW
    | LEFT_ARROW
    | FORALL
    | TYPE
    | FAT_RIGHT_ARROW
    | BACKSLASH
    | LEFT_CURLY_BRACKET
    | RIGHT_CURLY_BRACKET
    | LEFT_SQUARE_BRACKET
    | RIGHT_SQUARE_BRACKET
    | LEFT_QUOTE_BRACKET
    | RIGHT_QUOTE_BRACKET
    | LEFT_PAREN
    | RIGHT_PAREN
    | NIL
    | LET
    | OF
    | TYPE_ANNOTATION
    | UNIT
    | WHERE
    | WITH
    | WILD
    | COMMA
    | IN
    | IF
    | THEN
    | ELSE
    | DO
    | WHILE
    | CASE
    | CLASS
    | INST
    | DATA
    | NEWTYPE
    | PACKAGE
    | IMPORT
    | DOT
    | INFIX
    | INFIXL
    | INFIXR
    | RULE
    | SEMICOLON
    | OBJECT
    | LAYOUT_START
    | LAYOUT_SEPARATOR
    | LAYOUT_END
    | STRING of string
    | LOWER_IDENT of string
    | UPPER_IDENT of string
    | OPERATOR of string
    | INT of int
    | PRIM_INT of int
    | CHAR of Uchar.t
    | PRIM_CHAR of Uchar.t
    | FLOAT of float
    | PRIM_FLOAT of float