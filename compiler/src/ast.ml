module Export = struct
    type t =
        | Group of string * string list
        | Whole_group of string
        | Group_name of string
        | Definition of string
end

module Import = struct
    type term =
        | Group of string * string list
        | Whole_group of string
        | Group_name of string
        | Definition of string

    type t =
        | Whole_pkg of string
        | From_pkg of string * term list
end

module S = struct
    type t = string * Export.t list
end

module T = struct
    type t =
        | Import of Import.t
end

module P = struct
    type t = S.t * T.t list
end