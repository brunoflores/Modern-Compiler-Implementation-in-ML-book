(** Traversal function that uses environments to map variables to bindings.
    The binding is [bool ref].

    Whenever a variable or formal parameter declaration is found at
    static function-nesting depth [d], such as

    {v VarDec{name=symbol("a"); escape=r;...} v}

    then the [bool ref r] is set to [false]; the binding is entered in the
    environment. This new environment is used in processing expressions within
    the scope of the variable; whenever [a] is used at depth > [d], then [r]
    is set to true. *)
let find_escape (_ : Tiger.exp) : unit = ()
