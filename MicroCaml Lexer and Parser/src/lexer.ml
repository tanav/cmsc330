open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
  
  let rec make_list str pos = 
    if pos >= String.length str then [] else
        
      if Str.string_match (Str.regexp "[0-9]+") str pos then
        let temp = Str.matched_string str in
        Tok_Int(int_of_string temp) :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "(-[0-9]+)") str pos then
        let temp = Str.matched_string str in
        let temp2 = String.sub temp 1 ((String.length temp) - 2) in
        Tok_Int(int_of_string temp2) :: make_list str (Str.match_end())
      
      else if Str.string_match (Str.regexp "->") str pos then
        Tok_Arrow :: make_list str (Str.match_end())
      
      else if Str.string_match (Str.regexp ")") str pos then
        Tok_RParen :: make_list str (Str.match_end())
        
      else if Str.string_match (Str.regexp "(") str pos then
        Tok_LParen :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "=") str pos then
        Tok_Equal :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "<>") str pos then
        Tok_NotEqual :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp ">") str pos then
        Tok_Greater :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "<") str pos then
        Tok_Less :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp ">=") str pos then
        Tok_GreaterEqual :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "<=") str pos then
        Tok_LessEqual :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "||") str pos then
        Tok_Or :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "&&") str pos then
        Tok_And :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "+") str pos then
        Tok_Add :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "-") str pos then
        Tok_Sub :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "*") str pos then
        Tok_Mult :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "/") str pos then
        Tok_Div :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "\\^") str pos then
        Tok_Concat :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "not") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_Not :: make_list str loc

      else if Str.string_match (Str.regexp "if") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_If :: make_list str loc

      else if Str.string_match (Str.regexp "then") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_Then :: make_list str loc

      else if Str.string_match (Str.regexp "else") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_Else :: make_list str loc

      else if Str.string_match (Str.regexp "let") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_Let :: make_list str loc

      else if Str.string_match (Str.regexp "rec") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_Rec :: make_list str loc
      
      else if Str.string_match (Str.regexp "in") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_In :: make_list str loc

      else if Str.string_match (Str.regexp "def") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_Def :: make_list str loc

      else if Str.string_match (Str.regexp "fun") str pos then
        let temp = Str.matched_string str in
        let loc = Str.match_end() in
          if Str.string_match (Str.regexp "[a-zA-Z0-9]+") str loc then
            let temp2 = Str.matched_string str in 
            Tok_ID(temp ^ temp2) :: make_list str (Str.match_end())
          else Tok_Fun :: make_list str loc

      else if Str.string_match (Str.regexp "true\\|false") str pos then
        let temp = Str.matched_string str in
        Tok_Bool(bool_of_string temp) :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "\"\\([^\"]*\\)\"") str pos then
        let temp = Str.matched_string str in
        let temp2 = Str.matched_group 1 str in
        Tok_String(temp2) :: make_list str (String.length temp + pos)

      else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str pos then
        let temp = Str.matched_string str in
        Tok_ID(temp) :: make_list str (String.length temp + pos)

      else if Str.string_match (Str.regexp ";;") str pos then
        Tok_DoubleSemi :: make_list str (Str.match_end())

      else if Str.string_match (Str.regexp "[ \t\n]+") str pos then
        make_list str (Str.match_end())

      else raise (InvalidInputException "ERROR!")

    in make_list input 0