include Lexer_shared

let token = Dune_lexer.token
let jbuild_token = Jbuild_lexer.token

let choose = function
  | Atom.Dune -> token
  | Jbuild -> jbuild_token
