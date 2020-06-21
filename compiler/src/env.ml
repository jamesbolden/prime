type t = { mutable compile_object: bool;
           mutable compile_library: bool;
           mutable compile_prelude: bool;
           mutable imports: string list;
           mutable output_file: string option;
           mutable input_file: string option }

let env = { compile_object = false;
            compile_library = false;
            compile_prelude = false;
            imports = [];
            output_file = None;
            input_file = None }

module Env = struct
    let compile_object_flag = ref false
    let compile_library_flag = ref false
    let compile_prelude_flag = ref false
    let imports_ref = ref []
    let output_file_ref = ref None
    let input_file_ref = ref None
    let env_parsed = ref false
    
    let specs = ["-c", Arg.Set compile_object_flag, "compile an object instead of an executable";
                 "-a", Arg.Set compile_library_flag, "compile a library instead of an executable";
                 "--P", Arg.Set compile_prelude_flag, "indicates that the file being compiled is Prelude";
                 "-i", Arg.String (fun s -> imports_ref := s :: !imports_ref), "import a package";
                 "-o", Arg.String (fun s -> output_file_ref := Some s), "the output file"]
    
    let anon f = input_file_ref := Some f

    let get_args () =
        Arg.parse specs anon "pc [-c | -a] [--P] [-o output] input";
        env.compile_object <- !compile_object_flag;
        env.compile_library <- !compile_library_flag;
        env.compile_prelude <- !compile_prelude_flag;
        env.imports <- !imports_ref;
        env.output_file <- !output_file_ref;
        env.input_file <- !input_file_ref;
        env_parsed := true
end

let get_args () = Env.get_args ()