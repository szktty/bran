exception Unbound_value_error of Location.t * Id.t * Id.t list
exception Unbound_module_error of Location.t * Id.t * Id.t list

val f : Binding.t -> Ast_t.def list -> Ast_t.def list
