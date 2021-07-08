(**
   Boilerplate to be used as a template when mapping the bash CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_string_content (env : env) (tok : CST.string_content) =
  token env tok (* string_content *)

let map_simple_heredoc_body (env : env) (tok : CST.simple_heredoc_body) =
  token env tok (* simple_heredoc_body *)

let map_ansii_c_string (env : env) (tok : CST.ansii_c_string) =
  token env tok (* pattern "\\$'([^']|\\\\')*'" *)

let map_variable_name (env : env) (tok : CST.variable_name) =
  token env tok (* variable_name *)

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `SEMI tok -> token env tok (* ";" *)
  | `SEMISEMI tok -> token env tok (* ";;" *)
  | `LF tok -> token env tok (* "\n" *)
  | `AMP tok -> token env tok (* "&" *)
  )

let map_empty_value (env : env) (tok : CST.empty_value) =
  token env tok (* empty_value *)

let map_file_descriptor (env : env) (tok : CST.file_descriptor) =
  token env tok (* file_descriptor *)

let map_concat (env : env) (tok : CST.concat) =
  token env tok (* concat *)

let map_semgrep_metavariable_name (env : env) (tok : CST.semgrep_metavariable_name) =
  token env tok (* pattern [A-Z_][A-Z_0-9]* *)

let map_test_operator (env : env) (tok : CST.test_operator) =
  token env tok (* test_operator *)

let map_special_variable_name (env : env) (x : CST.special_variable_name) =
  (match x with
  | `STAR tok -> token env tok (* "*" *)
  | `AT tok -> token env tok (* "@" *)
  | `QMARK tok -> token env tok (* "?" *)
  | `DASH tok -> token env tok (* "-" *)
  | `DOLLAR tok -> token env tok (* "$" *)
  | `X_0 tok -> token env tok (* "0" *)
  | `X__ tok -> token env tok (* "_" *)
  )

let map_heredoc_body_beginning (env : env) (tok : CST.heredoc_body_beginning) =
  token env tok (* heredoc_body_beginning *)

let map_word (env : env) (tok : CST.word) =
  token env tok (* word *)

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  token env tok (* heredoc_start *)

let map_pat_42e353e (env : env) (tok : CST.pat_42e353e) =
  token env tok (* pattern \w+ *)

let map_raw_string (env : env) (tok : CST.raw_string) =
  token env tok (* pattern "'[^']*'" *)

let map_regex (env : env) (tok : CST.regex) =
  token env tok (* regex *)

let map_heredoc_body_end (env : env) (tok : CST.heredoc_body_end) =
  token env tok (* heredoc_body_end *)

let map_heredoc_body_middle (env : env) (tok : CST.heredoc_body_middle) =
  token env tok (* heredoc_body_middle *)

let map_special_character (env : env) (tok : CST.special_character) =
  token env tok (* special_character *)

let map_heredoc_redirect (env : env) ((v1, v2) : CST.heredoc_redirect) =
  let v1 =
    (match v1 with
    | `LTLT tok -> token env tok (* "<<" *)
    | `LTLTDASH tok -> token env tok (* "<<-" *)
    )
  in
  let v2 = token env v2 (* heredoc_start *) in
  todo env (v1, v2)

let map_simple_expansion (env : env) ((v1, v2) : CST.simple_expansion) =
  let v1 = token env v1 (* "$" *) in
  let v2 =
    (match v2 with
    | `Pat_42e353e tok -> token env tok (* pattern \w+ *)
    | `Choice_STAR x -> map_special_variable_name env x
    | `BANG tok -> token env tok (* "!" *)
    | `HASH tok -> token env tok (* "#" *)
    )
  in
  todo env (v1, v2)

let rec map_anon_choice_prim_exp_618725a (env : env) (x : CST.anon_choice_prim_exp_618725a) =
  (match x with
  | `Choice_semg_ellips x -> map_primary_expression env x
  | `Spec_char tok -> token env tok (* special_character *)
  )

and map_anon_stmt_opt_LF_here_body_term_3efa649 (env : env) ((v1, v2, v3) : CST.anon_stmt_opt_LF_here_body_term_3efa649) =
  let v1 = map_statement env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "\n" *) in
        let v2 = map_heredoc_body env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = map_terminator env v3 in
  todo env (v1, v2, v3)

and map_array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = token env v1 (* "(" *) in
  let v2 = List.map (map_literal env) v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_choice_EQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQ tok -> token env tok (* "=" *)
        | `EQEQ tok -> token env tok (* "==" *)
        | `EQTILDE tok -> token env tok (* "=~" *)
        | `BANGEQ tok -> token env tok (* "!=" *)
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `PLUSEQ tok -> token env tok (* "+=" *)
        | `DASHEQ tok -> token env tok (* "-=" *)
        | `LT tok -> token env tok (* "<" *)
        | `GT tok -> token env tok (* ">" *)
        | `LTEQ tok -> token env tok (* "<=" *)
        | `GTEQ tok -> token env tok (* ">=" *)
        | `BARBAR tok -> token env tok (* "||" *)
        | `AMPAMP tok -> token env tok (* "&&" *)
        | `Test_op tok -> token env tok (* test_operator *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_EQEQ_regex (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> token env tok (* "==" *)
        | `EQTILDE tok -> token env tok (* "=~" *)
        )
      in
      let v3 = token env v3 (* regex *) in
      todo env (v1, v2, v3)
  )

and map_case_item (env : env) ((v1, v2, v3, v4, v5) : CST.case_item) =
  let v1 = map_literal env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "|" *) in
      let v2 = map_literal env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 = token env v3 (* ")" *) in
  let v4 = map_program env v4 in
  let v5 =
    (match v5 with
    | `SEMISEMI tok -> token env tok (* ";;" *)
    | `Choice_SEMIAMP x ->
        (match x with
        | `SEMIAMP tok -> token env tok (* ";&" *)
        | `SEMISEMIAMP tok -> token env tok (* ";;&" *)
        )
    )
  in
  todo env (v1, v2, v3, v4, v5)

and map_command (env : env) ((v1, v2, v3) : CST.command) =
  let v1 =
    List.map (fun x ->
      (match x with
      | `Var_assign x -> map_variable_assignment env x
      | `File_redi x -> map_file_redirect env x
      )
    ) v1
  in
  let v2 = map_command_name env v2 in
  let v3 =
    List.map (fun x ->
      (match x with
      | `Choice_conc x -> map_literal env x
      | `Choice_EQTILDE_choice_choice_conc (v1, v2) ->
          let v1 =
            (match v1 with
            | `EQTILDE tok -> token env tok (* "=~" *)
            | `EQEQ tok -> token env tok (* "==" *)
            )
          in
          let v2 =
            (match v2 with
            | `Choice_conc x -> map_literal env x
            | `Regex tok -> token env tok (* regex *)
            )
          in
          todo env (v1, v2)
      )
    ) v3
  in
  todo env (v1, v2, v3)

and map_command_name (env : env) (x : CST.command_name) =
  (match x with
  | `Conc x -> map_concatenation env x
  | `Choice_semg_ellips x -> map_primary_expression env x
  | `Rep1_spec_char xs ->
      List.map (token env) (* special_character *) xs
  )

and map_command_substitution (env : env) (x : CST.command_substitution) =
  (match x with
  | `DOLLARLPAR_stmts_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "$(" *) in
      let v2 = map_statements env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `DOLLARLPAR_file_redi_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "$(" *) in
      let v2 = map_file_redirect env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `BQUOT_stmts_BQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "`" *) in
      let v2 = map_statements env v2 in
      let v3 = token env v3 (* "`" *) in
      todo env (v1, v2, v3)
  )

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_statements2 env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_concatenation (env : env) ((v1, v2, v3) : CST.concatenation) =
  let v1 = map_anon_choice_prim_exp_618725a env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* concat *) in
      let v2 = map_anon_choice_prim_exp_618725a env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* concat *) in
        let v2 = token env v2 (* "$" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_do_group (env : env) ((v1, v2, v3) : CST.do_group) =
  let v1 = token env v1 (* "do" *) in
  let v2 =
    (match v2 with
    | Some x -> map_statements2 env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "done" *) in
  todo env (v1, v2, v3)

and map_elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = token env v1 (* "elif" *) in
  let v2 = map_terminated_statement env v2 in
  let v3 = token env v3 (* "then" *) in
  let v4 =
    (match v4 with
    | Some x -> map_statements2 env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = token env v1 (* "else" *) in
  let v2 =
    (match v2 with
    | Some x -> map_statements2 env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_expansion (env : env) ((v1, v2, v3, v4) : CST.expansion) =
  let v1 = token env v1 (* "${" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `HASH tok -> token env tok (* "#" *)
        | `BANG tok -> token env tok (* "!" *)
        )
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Var_name_EQ_opt_choice_conc (v1, v2, v3) ->
            let v1 = token env v1 (* variable_name *) in
            let v2 = token env v2 (* "=" *) in
            let v3 =
              (match v3 with
              | Some x -> map_literal env x
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | `Choice_subs_opt_SLASH_opt_regex_rep_choice_choice_conc (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | `Subs x -> map_subscript env x
              | `Pat_42e353e tok -> token env tok (* pattern \w+ *)
              | `Choice_STAR x -> map_special_variable_name env x
              )
            in
            let v2 =
              (match v2 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* / *) in
                  let v2 =
                    (match v2 with
                    | Some tok -> token env tok (* regex *)
                    | None -> todo env ())
                  in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            let v3 =
              List.map (fun x ->
                (match x with
                | `Choice_conc x -> map_literal env x
                | `COLON tok -> token env tok (* ":" *)
                | `COLONQMARK tok -> token env tok (* ":?" *)
                | `EQ tok -> token env tok (* "=" *)
                | `COLONDASH tok -> token env tok (* ":-" *)
                | `PERC tok -> token env tok (* "%" *)
                | `DASH tok -> token env tok (* "-" *)
                | `HASH tok -> token env tok (* "#" *)
                )
              ) v3
            in
            todo env (v1, v2, v3)
        )
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_conc x -> map_literal env x
  | `Un_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `BANG tok -> token env tok (* "!" *)
        | `Test_op tok -> token env tok (* test_operator *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ":" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Bin_exp x -> map_binary_expression env x
  | `Post_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSPLUS tok -> token env tok (* "++" *)
        | `DASHDASH tok -> token env tok (* "--" *)
        )
      in
      todo env (v1, v2)
  | `Paren_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )

and map_file_redirect (env : env) ((v1, v2, v3) : CST.file_redirect) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* file_descriptor *)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `LT tok -> token env tok (* "<" *)
    | `GT tok -> token env tok (* ">" *)
    | `GTGT tok -> token env tok (* ">>" *)
    | `AMPGT tok -> token env tok (* "&>" *)
    | `AMPGTGT tok -> token env tok (* "&>>" *)
    | `LTAMP tok -> token env tok (* "<&" *)
    | `GTAMP tok -> token env tok (* ">&" *)
    | `GTBAR tok -> token env tok (* ">|" *)
    )
  in
  let v3 = map_literal env v3 in
  todo env (v1, v2, v3)

and map_heredoc_body (env : env) (x : CST.heredoc_body) =
  (match x with
  | `Simple_here_body tok ->
      token env tok (* simple_heredoc_body *)
  | `Here_body_begin_rep_choice_expa_here_body_end (v1, v2, v3) ->
      let v1 = token env v1 (* heredoc_body_beginning *) in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Expa x -> map_expansion env x
          | `Simple_expa x -> map_simple_expansion env x
          | `Cmd_subs x -> map_command_substitution env x
          | `Here_body_middle tok ->
              token env tok (* heredoc_body_middle *)
          )
        ) v2
      in
      let v3 = token env v3 (* heredoc_body_end *) in
      todo env (v1, v2, v3)
  )

and map_herestring_redirect (env : env) ((v1, v2) : CST.herestring_redirect) =
  let v1 = token env v1 (* "<<<" *) in
  let v2 = map_literal env v2 in
  todo env (v1, v2)

and map_last_case_item (env : env) ((v1, v2, v3, v4, v5) : CST.last_case_item) =
  let v1 = map_literal env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "|" *) in
      let v2 = map_literal env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 = token env v3 (* ")" *) in
  let v4 = map_program env v4 in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* ";;" *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Conc x -> map_concatenation env x
  | `Choice_semg_ellips x -> map_primary_expression env x
  | `Rep1_spec_char xs ->
      List.map (token env) (* special_character *) xs
  )

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Semg_ellips tok -> token env tok (* "..." *)
  | `Semg_double_curl_meta (v1, v2, v3) ->
      let v1 = token env v1 (* "${{" *) in
      let v2 = token env v2 (* pattern [A-Z_][A-Z_0-9]* *) in
      let v3 = token env v3 (* "}}" *) in
      todo env (v1, v2, v3)
  | `Word tok -> token env tok (* word *)
  | `Str x -> map_string_ env x
  | `Raw_str tok -> token env tok (* pattern "'[^']*'" *)
  | `Ansii_c_str tok ->
      token env tok (* pattern "\\$'([^']|\\\\')*'" *)
  | `Expa x -> map_expansion env x
  | `Simple_expa x -> map_simple_expansion env x
  | `Str_expa (v1, v2) ->
      let v1 = token env v1 (* "$" *) in
      let v2 =
        (match v2 with
        | `Str x -> map_string_ env x
        | `Raw_str tok -> token env tok (* pattern "'[^']*'" *)
        )
      in
      todo env (v1, v2)
  | `Cmd_subs x -> map_command_substitution env x
  | `Proc_subs (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `LTLPAR tok -> token env tok (* "<(" *)
        | `GTLPAR tok -> token env tok (* ">(" *)
        )
      in
      let v2 = map_statements env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )

and map_program (env : env) (opt : CST.program) =
  (match opt with
  | Some x -> map_statements env x
  | None -> todo env ())

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Redi_stmt (v1, v2) ->
      let v1 = map_statement env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `File_redi x -> map_file_redirect env x
          | `Here_redi_a9657de x -> map_heredoc_redirect env x
          | `Here_redi_7d3292d x -> map_herestring_redirect env x
          )
        ) v2
      in
      todo env (v1, v2)
  | `Var_assign x -> map_variable_assignment env x
  | `Cmd x -> map_command env x
  | `Decl_cmd (v1, v2) ->
      let v1 =
        (match v1 with
        | `Decl tok -> token env tok (* "declare" *)
        | `Type tok -> token env tok (* "typeset" *)
        | `Export tok -> token env tok (* "export" *)
        | `Read tok -> token env tok (* "readonly" *)
        | `Local tok -> token env tok (* "local" *)
        )
      in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Choice_conc x -> map_literal env x
          | `Pat_42e353e tok -> token env tok (* pattern \w+ *)
          | `Var_assign x -> map_variable_assignment env x
          )
        ) v2
      in
      todo env (v1, v2)
  | `Unset_cmd (v1, v2) ->
      let v1 =
        (match v1 with
        | `Unset tok -> token env tok (* "unset" *)
        | `Unse tok -> token env tok (* "unsetenv" *)
        )
      in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Choice_conc x -> map_literal env x
          | `Pat_42e353e tok -> token env tok (* pattern \w+ *)
          )
        ) v2
      in
      todo env (v1, v2)
  | `Test_cmd x -> map_test_command env x
  | `Nega_cmd (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 =
        (match v2 with
        | `Cmd x -> map_command env x
        | `Test_cmd x -> map_test_command env x
        | `Subs x -> map_subshell env x
        )
      in
      todo env (v1, v2)
  | `For_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* pattern \w+ *) in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "in" *) in
            let v2 = List.map (map_literal env) v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 = map_terminator env v4 in
      let v5 = map_do_group env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `C_style_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "((" *) in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 = map_terminator env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v6 = map_terminator env v6 in
      let v7 =
        (match v7 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v8 = token env v8 (* "))" *) in
      let v9 =
        (match v9 with
        | Some tok -> token env tok (* ";" *)
        | None -> todo env ())
      in
      let v10 =
        (match v10 with
        | `Do_group x -> map_do_group env x
        | `Comp_stmt x -> map_compound_statement env x
        )
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_terminated_statement env v2 in
      let v3 = map_do_group env v3 in
      todo env (v1, v2, v3)
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = map_terminated_statement env v2 in
      let v3 = token env v3 (* "then" *) in
      let v4 =
        (match v4 with
        | Some x -> map_statements2 env x
        | None -> todo env ())
      in
      let v5 = List.map (map_elif_clause env) v5 in
      let v6 =
        (match v6 with
        | Some x -> map_else_clause env x
        | None -> todo env ())
      in
      let v7 = token env v7 (* "fi" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Case_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "case" *) in
      let v2 = map_literal env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_terminator env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "in" *) in
      let v5 = map_terminator env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = List.map (map_case_item env) v1 in
            let v2 = map_last_case_item env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = token env v7 (* "esac" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Pipe (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 =
        (match v2 with
        | `BAR tok -> token env tok (* "|" *)
        | `BARAMP tok -> token env tok (* "|&" *)
        )
      in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `List (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 =
        (match v2 with
        | `AMPAMP tok -> token env tok (* "&&" *)
        | `BARBAR tok -> token env tok (* "||" *)
        )
      in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Subs x -> map_subshell env x
  | `Comp_stmt x -> map_compound_statement env x
  | `Func_defi (v1, v2) ->
      let v1 =
        (match v1 with
        | `Func_word_opt_LPAR_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "function" *) in
            let v2 = token env v2 (* word *) in
            let v3 =
              (match v3 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* "(" *) in
                  let v2 = token env v2 (* ")" *) in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | `Word_LPAR_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* word *) in
            let v2 = token env v2 (* "(" *) in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      let v2 =
        (match v2 with
        | `Comp_stmt x -> map_compound_statement env x
        | `Subs x -> map_subshell env x
        | `Test_cmd x -> map_test_command env x
        )
      in
      todo env (v1, v2)
  )

and map_statements (env : env) ((v1, v2, v3, v4) : CST.statements) =
  let v1 =
    List.map (map_anon_stmt_opt_LF_here_body_term_3efa649 env) v1
  in
  let v2 = map_statement env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "\n" *) in
        let v2 = map_heredoc_body env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_terminator env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_statements2 (env : env) (xs : CST.statements2) =
  List.map (map_anon_stmt_opt_LF_here_body_term_3efa649 env) xs

and map_string_ (env : env) ((v1, v2, v3, v4) : CST.string_) =
  let v1 = token env v1 (* "\"" *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Opt_DOLLAR_str_content (v1, v2) ->
            let v1 =
              (match v1 with
              | Some tok -> token env tok (* "$" *)
              | None -> todo env ())
            in
            let v2 = token env v2 (* string_content *) in
            todo env (v1, v2)
        | `Expa x -> map_expansion env x
        | `Simple_expa x -> map_simple_expansion env x
        | `Cmd_subs x -> map_command_substitution env x
        )
      in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* concat *)
        | None -> todo env ())
      in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "$" *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "\"" *) in
  todo env (v1, v2, v3, v4)

and map_subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let v1 = token env v1 (* variable_name *) in
  let v2 = token env v2 (* "[" *) in
  let v3 = map_literal env v3 in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* concat *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* "]" *) in
  let v6 =
    (match v6 with
    | Some tok -> token env tok (* concat *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and map_subshell (env : env) ((v1, v2, v3) : CST.subshell) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_statements env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_terminated_statement (env : env) ((v1, v2) : CST.terminated_statement) =
  let v1 = map_statement env v1 in
  let v2 = map_terminator env v2 in
  todo env (v1, v2)

and map_test_command (env : env) (v1 : CST.test_command) =
  (match v1 with
  | `LBRACK_exp_RBRACK (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  | `LBRACKLBRACK_exp_RBRACKRBRACK (v1, v2, v3) ->
      let v1 = token env v1 (* "[[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]]" *) in
      todo env (v1, v2, v3)
  | `LPARLPAR_exp_RPARRPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "((" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "))" *) in
      todo env (v1, v2, v3)
  )

and map_variable_assignment (env : env) ((v1, v2, v3) : CST.variable_assignment) =
  let v1 =
    (match v1 with
    | `Var_name tok -> token env tok (* variable_name *)
    | `Subs x -> map_subscript env x
    )
  in
  let v2 =
    (match v2 with
    | `EQ tok -> token env tok (* "=" *)
    | `PLUSEQ tok -> token env tok (* "+=" *)
    )
  in
  let v3 =
    (match v3 with
    | `Choice_conc x -> map_literal env x
    | `Array x -> map_array_ env x
    | `Empty_value tok -> token env tok (* empty_value *)
    )
  in
  todo env (v1, v2, v3)
