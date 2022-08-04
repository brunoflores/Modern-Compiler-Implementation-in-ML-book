module type I = sig
  type key
  type 'a table

  val empty : 'a table
  val enter : key -> 'a -> 'a table -> 'a table
  val look : key -> 'a table -> 'a option
end
