type term =
  | Abs(string,term)
  | LAbs (string,term)
  | App (term,term)
