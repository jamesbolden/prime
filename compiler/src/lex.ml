open Sedlexing
open Token

module Lex = struct
    let layout = ref []
    let waiting = ref false
    let when_newline = ref None

    let add_line posn lexbuf = { posn with
                                 Lexing.pos_lnum = posn.Lexing.pos_lnum + 1;
                                 Lexing.pos_bol = lexeme_end lexbuf;
                                 Lexing.pos_cnum = 0 }
    
    let upd_posn posn lexbuf = { posn with
                                 Lexing.pos_cnum = posn.Lexing.pos_cnum + lexeme_length lexbuf }

    let action posn buf f =
        if !waiting
        then match !when_newline with
            | Some l -> if posn.Lexing.pos_lnum > l
                    then (waiting := false; when_newline := None; let old = posn.Lexing.pos_cnum in layout := old :: !layout; (upd_posn posn buf, LAYOUT_START :: f buf))
                    else (waiting := false; when_newline := None; (upd_posn posn buf, f buf))
            | None -> waiting := false; let old = posn.Lexing.pos_cnum in layout := old :: !layout; (upd_posn posn buf, LAYOUT_START :: f buf)
        else upd_posn posn buf, f buf
    
    type ord =
        | Lt
        | Eq
        | Gt
    
    let offside ind _ = match !layout with
        | [] -> Gt
        | x :: _ ->
            if ind < x
            then Lt
            else if ind = x
            then Eq
            else Gt
    
    let pop_context _ = match !layout with
        | [] -> ()
        | _ :: xs -> layout := xs
    
    let show_posn p = p.Lexing.pos_fname ^ ":" ^ string_of_int p.Lexing.pos_lnum ^ ":" ^ string_of_int p.Lexing.pos_cnum
end

let re_lident = [%sedlex.regexp? lowercase, Star (id_continue | '0'..'9'), Opt '#']
let re_uident = [%sedlex.regexp? uppercase, Star (id_continue | '0'..'9'), Opt '#']
let re_symbol = [%sedlex.regexp? math | other_math | '!' | '$' | '%' | '&' | '|' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '^' | '~' | '@' | '\\' | '#' | '.' | ':' | '?']
let re_digit = [%sedlex.regexp? '0'..'9']
let re_int = [%sedlex.regexp? Opt '-', Plus ('0'..'9')]
let re_char = [%sedlex.regexp? '\'', any, '\'']
let re_brk_char = [%sedlex.regexp? '\'', '\\', any, '\'']
let re_float = [%sedlex.regexp? Opt '-', Plus ('0'..'9'), '.', Plus ('0'..'9')]
let re_prim_int = [%sedlex.regexp? re_int, '#']
let re_prim_char = [%sedlex.regexp? re_char, '#']
let re_prim_brk_char = [%sedlex.regexp? re_brk_char, '#']
let re_prim_float = [%sedlex.regexp? re_float, '#']
let re_comment = [%sedlex.regexp? ';', ';', Star (Compl '\n')]
let re_white = [%sedlex.regexp? Star (Intersect (white_space, Compl '\n'))]
let re_nonempty_white = [%sedlex.regexp? Plus (Intersect (white_space, Compl '\n'))]
let re_ndq = [%sedlex.regexp? Star (Compl '\"')]
let re_string = [%sedlex.regexp? '\"', re_ndq, '\"']

let rec token_bol ind posn lexbuf =
    match%sedlex lexbuf with
        | '\n', re_nonempty_white -> let nposn = { posn with
                                                   Lexing.pos_lnum = posn.Lexing.pos_lnum + 1;
                                                   Lexing.pos_bol = lexeme_start lexbuf + 1;
                                                   Lexing.pos_cnum = lexeme_length lexbuf - 1 }
            in token_bol (lexeme_length lexbuf - 1) nposn lexbuf
        | '\n' ->
            let _ = Sedlexing.mark lexbuf in
            token_newline (Lex.add_line posn lexbuf) lexbuf
        | re_comment, '\n' -> token (Lex.add_line posn lexbuf) lexbuf
        | re_nonempty_white -> token (Lex.upd_posn posn lexbuf) lexbuf
        | _ ->
            let rec handle_offside_rule () =
                match Lex.offside ind lexbuf with
                    | Lex.Lt ->
                        Lex.pop_context ();
                        let p, ts = handle_offside_rule () in
                        p, LAYOUT_END :: ts
                    | Lex.Eq ->
                        let p, ts = token posn lexbuf in
                        p, LAYOUT_SEPARATOR :: ts
                    | Lex.Gt -> token posn lexbuf
            in handle_offside_rule ()
and token_newline posn lexbuf =
    match%sedlex lexbuf with
        | Compl '\n' ->
            let rec emit_dedents ts = match !Lex.layout with
                | [] -> ts
                | _ :: xs -> Lex.layout := xs; emit_dedents (LAYOUT_END :: ts)
            in
            let p, t =
                let _ = Sedlexing.backtrack lexbuf in
                token posn lexbuf
            in p, emit_dedents t
        | _ ->
            let _ = Sedlexing.backtrack lexbuf in
            token posn lexbuf
and token posn lexbuf =
    match%sedlex lexbuf with
        | '\n', re_nonempty_white -> let nposn = { posn with
                                                   Lexing.pos_lnum = posn.Lexing.pos_lnum + 1;
                                                   Lexing.pos_bol = lexeme_start lexbuf + 1;
                                                   Lexing.pos_cnum = lexeme_length lexbuf - 1 }
            in token_bol (lexeme_length lexbuf - 1) nposn lexbuf
        | '\n' ->
            let _ = Sedlexing.mark lexbuf in
            token_newline (Lex.add_line posn lexbuf) lexbuf
        | re_comment, '\n' -> token (Lex.add_line posn lexbuf) lexbuf
        | re_nonempty_white -> token (Lex.upd_posn posn lexbuf) lexbuf
        | "let" -> Lex.action posn lexbuf (fun _ -> Lex.waiting := true; Lex.when_newline := Some posn.Lexing.pos_lnum; [LET])
        | "do" -> Lex.action posn lexbuf (fun _ -> Lex.waiting := true; Lex.when_newline := Some posn.Lexing.pos_lnum; [DO])
        | "=" -> Lex.action posn lexbuf (fun _ -> Lex.waiting := true; Lex.when_newline := Some posn.Lexing.pos_lnum; [DEF])
        | "where" -> Lex.action posn lexbuf (fun _ -> Lex.waiting := true; [WHERE])
        | "of" -> Lex.action posn lexbuf (fun _ -> Lex.waiting := true; [OF])
        | ";" -> Lex.action posn lexbuf (fun _ -> [SEMICOLON])
        | "->" -> Lex.action posn lexbuf (fun _ -> [RIGHT_ARROW])
        | "<-" -> Lex.action posn lexbuf (fun _ -> [LEFT_ARROW])
        | "=>" -> Lex.action posn lexbuf (fun _ -> [FAT_RIGHT_ARROW])
        | "::" -> Lex.action posn lexbuf (fun _ -> [TYPE_ANNOTATION])
        | "()" -> Lex.action posn lexbuf (fun _ -> [UNIT])
        | "[]" -> Lex.action posn lexbuf (fun _ -> [NIL])
        | "(" -> Lex.action posn lexbuf (fun _ -> [LEFT_PAREN])
        | ")" -> Lex.action posn lexbuf (fun _ -> [RIGHT_PAREN])
        | "[" -> Lex.action posn lexbuf (fun _ -> [LEFT_SQUARE_BRACKET])
        | "]" -> Lex.action posn lexbuf (fun _ -> [RIGHT_SQUARE_BRACKET])
        | "{" -> Lex.action posn lexbuf (fun _ -> [LEFT_CURLY_BRACKET])
        | "}" -> Lex.action posn lexbuf (fun _ -> [RIGHT_CURLY_BRACKET])
        | "[|" -> Lex.action posn lexbuf (fun _ -> [LEFT_QUOTE_BRACKET])
        | "|]" -> Lex.action posn lexbuf (fun _ -> [RIGHT_QUOTE_BRACKET])
        | "_" -> Lex.action posn lexbuf (fun _ -> [WILD])
        | "," -> Lex.action posn lexbuf (fun _ -> [COMMA])
        | "\\" -> Lex.action posn lexbuf (fun _ -> [BACKSLASH])
        | "`" -> Lex.action posn lexbuf (fun _ -> [BACKTICK])
        | "|" -> Lex.action posn lexbuf (fun _ -> [BAR])
        | "." -> Lex.action posn lexbuf (fun _ -> [DOT])
        | ".." -> Lex.action posn lexbuf (fun _ -> [ELLIPSES])
        | "if" -> Lex.action posn lexbuf (fun _ -> [IF])
        | "then" -> Lex.action posn lexbuf (fun _ -> [THEN])
        | "else" -> Lex.action posn lexbuf (fun _ -> [ELSE])
        | "while" -> Lex.action posn lexbuf (fun _ -> [WHILE])
        | "with" -> Lex.action posn lexbuf (fun _ -> [WITH])
        | "class" -> Lex.action posn lexbuf (fun _ -> [CLASS])
        | "instance" -> Lex.action posn lexbuf (fun _ -> [INST])
        | "object" -> Lex.action posn lexbuf (fun _ -> [OBJECT])
        | "data" -> Lex.action posn lexbuf (fun _ -> [DATA])
        | "newtype" -> Lex.action posn lexbuf (fun _ -> [NEWTYPE])
        | "type" -> Lex.action posn lexbuf (fun _ -> [TYPE])
        | "forall" -> Lex.action posn lexbuf (fun _ -> [FORALL])
        | "package" -> Lex.action posn lexbuf (fun _ -> [PACKAGE])
        | "rule" -> Lex.action posn lexbuf (fun _ -> [RULE])
        | "infix" -> Lex.action posn lexbuf (fun _ -> [INFIX])
        | "infixl" -> Lex.action posn lexbuf (fun _ -> [INFIXL])
        | "infixr" -> Lex.action posn lexbuf (fun _ -> [INFIXR])
        | Plus re_symbol -> Lex.action posn lexbuf (fun lb -> [OPERATOR (Utf8.lexeme lb)])
        | re_lident -> Lex.action posn lexbuf (fun lb -> [LOWER_IDENT (Utf8.lexeme lb)])
        | re_uident -> Lex.action posn lexbuf (fun lb -> [UPPER_IDENT (Utf8.lexeme lb)])
        | re_string -> Lex.action posn lexbuf (fun lb -> let s = Utf8.lexeme lb in [STRING (String.sub s 1 (String.length s - 2))])
        | re_brk_char -> Lex.action posn lexbuf (fun lb -> match Uchar.to_char (lexeme_char lb 2) with
            | 'n' -> [CHAR (Uchar.of_char '\n')]
            | 't' -> [CHAR (Uchar.of_char '\t')]
            | 'r' -> [CHAR (Uchar.of_char '\r')]
            | _ -> failwith "Staaahp")
        | re_char -> Lex.action posn lexbuf (fun lb -> [CHAR (lexeme_char lb 1)])
        | re_int -> Lex.action posn lexbuf (fun lb -> [INT (int_of_string (Utf8.lexeme lb))])
        | re_float -> Lex.action posn lexbuf (fun lb -> [FLOAT (float_of_string (Utf8.lexeme lb))])
        | re_prim_brk_char -> Lex.action posn lexbuf (fun lb -> [PRIM_CHAR (lexeme_char lb 2)])
        | re_prim_char -> Lex.action posn lexbuf (fun lb -> [PRIM_CHAR (lexeme_char lb 1)])
        | re_prim_int -> Lex.action posn lexbuf (fun lb -> let s = Utf8.lexeme lb in [PRIM_INT (int_of_string (Util.strip_last_char s))])
        | re_prim_float -> Lex.action posn lexbuf (fun lb -> let s = Utf8.lexeme lb in [PRIM_FLOAT (float_of_string (Util.strip_last_char s))])
        | Star white_space, eof ->
            let rec emit_dedents ts = match !Lex.layout with
                | [] -> Lex.upd_posn posn lexbuf, ts
                | _ :: xs -> Lex.layout := xs; emit_dedents (LAYOUT_END :: ts)
            in emit_dedents [EOF]
        | any -> failwith ("lexing error--unexpected character: " ^ Utf8.lexeme lexbuf)
        | _ -> failwith "impossible lexing error"

