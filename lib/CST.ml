(* Generated by ocaml-tree-sitter. *)
(*
   bash grammar

   entrypoint: program
*)

open! Sexplib.Conv
open Tree_sitter_run

type semgrep_metavariable = Token.t (* pattern \$[A-Z_][A-Z_0-9]* *)

type string_content = Token.t

type empty_value = Token.t

type word = Token.t

type raw_string = Token.t (* pattern "'[^']*'" *)

type regex = Token.t

type heredoc_body_end = Token.t

type ansii_c_string = Token.t (* pattern "\\$'([^']|\\\\')*'" *)

type special_character = Token.t

type concat = Token.t

type semgrep_metavar_eq = Token.t (* pattern \$[A-Z_][A-Z_0-9]*= *)

type special_variable_name = [
    `STAR of Token.t (* "*" *)
  | `AT of Token.t (* "@" *)
  | `QMARK of Token.t (* "?" *)
  | `DASH of Token.t (* "-" *)
  | `DOLLAR of Token.t (* "$" *)
  | `X_0 of Token.t (* "0" *)
  | `X__ of Token.t (* "_" *)
]

type simple_heredoc_body = Token.t

type terminator = [
    `SEMI of Token.t (* ";" *)
  | `SEMISEMI of Token.t (* ";;" *)
  | `LF of Token.t (* "\n" *)
  | `AMP of Token.t (* "&" *)
]

type heredoc_start = Token.t

type heredoc_body_middle = Token.t

type pat_42e353e = Token.t (* pattern \w+ *)

type semgrep_named_ellipsis = Token.t (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *)

type semgrep_metavar_pluseq = Token.t (* pattern \$[A-Z_][A-Z_0-9]*\+= *)

type file_descriptor = Token.t

type test_operator = Token.t

type tok_prec_p1_slash = Token.t

type heredoc_body_beginning = Token.t

type variable_name = Token.t

type extended_word = [
    `Semg_meta of semgrep_metavariable (*tok*)
  | `Word of word (*tok*)
]

type heredoc_redirect = (
    [ `LTLT of Token.t (* "<<" *) | `LTLTDASH of Token.t (* "<<-" *) ]
  * heredoc_start (*tok*)
)

type orig_simple_variable_name = pat_42e353e

type simple_variable_name = [
    `Semg_meta of semgrep_metavariable (*tok*)
  | `Pat_42e353e of orig_simple_variable_name
]

type simple_expansion = [
    `DOLLAR_choice_orig_simple_var_name of (
        Token.t (* "$" *)
      * [
            `Orig_simple_var_name of orig_simple_variable_name
          | `Choice_STAR of special_variable_name
          | `BANG of Token.t (* "!" *)
          | `HASH of Token.t (* "#" *)
        ]
    )
  | `Semg_named_ellips of semgrep_named_ellipsis (*tok*)
]

type anon_choice_lit_bbf16c7 = [
    `Choice_conc of literal
  | `Array of (
        Token.t (* "(" *)
      * literal list (* zero or more *)
      * Token.t (* ")" *)
    )
  | `Empty_value of empty_value (*tok*)
]

and anon_choice_prim_exp_65e2c2e = [
    `Choice_semg_deep_exp of primary_expression
  | `Spec_char of special_character (*tok*)
]

and anon_stmt_opt_LF_here_body_term_3efa649 = (
    statement
  * (Token.t (* "\n" *) * heredoc_body) option
  * terminator
)

and binary_expression = [
    `Exp_choice_EQ_exp of (
        expression
      * [
            `EQ of Token.t (* "=" *)
          | `EQEQ of Token.t (* "==" *)
          | `EQTILDE of Token.t (* "=~" *)
          | `BANGEQ of Token.t (* "!=" *)
          | `PLUS of Token.t (* "+" *)
          | `DASH of Token.t (* "-" *)
          | `PLUSEQ of Token.t (* "+=" *)
          | `DASHEQ of Token.t (* "-=" *)
          | `LT of Token.t (* "<" *)
          | `GT of Token.t (* ">" *)
          | `LTEQ of Token.t (* "<=" *)
          | `GTEQ of Token.t (* ">=" *)
          | `BARBAR of Token.t (* "||" *)
          | `AMPAMP of Token.t (* "&&" *)
          | `Test_op of test_operator (*tok*)
        ]
      * expression
    )
  | `Exp_choice_EQEQ_regex of (
        expression
      * [ `EQEQ of Token.t (* "==" *) | `EQTILDE of Token.t (* "=~" *) ]
      * regex (*tok*)
    )
]

and case_item = (
    literal
  * (Token.t (* "|" *) * literal) list (* zero or more *)
  * Token.t (* ")" *)
  * program
  * [
        `SEMISEMI of Token.t (* ";;" *)
      | `Choice_SEMIAMP of [
            `SEMIAMP of Token.t (* ";&" *)
          | `SEMISEMIAMP of Token.t (* ";;&" *)
        ]
    ]
)

and command = (
    [ `Var_assign of variable_assignment | `File_redi of file_redirect ]
      list (* zero or more *)
  * command_name
  * [
        `Choice_conc of literal
      | `Choice_EQTILDE_choice_choice_conc of (
            [ `EQTILDE of Token.t (* "=~" *) | `EQEQ of Token.t (* "==" *) ]
          * [ `Choice_conc of literal | `Regex of regex (*tok*) ]
        )
    ]
      list (* zero or more *)
)

and command_name = [
    `Conc of concatenation
  | `Choice_semg_deep_exp of primary_expression
  | `Rep1_spec_char of special_character (*tok*) list (* one or more *)
]

and command_substitution = [
    `DOLLARLPAR_stmts_RPAR of (
        Token.t (* "$(" *) * statements * Token.t (* ")" *)
    )
  | `DOLLARLPAR_file_redi_RPAR of (
        Token.t (* "$(" *) * file_redirect * Token.t (* ")" *)
    )
  | `BQUOT_stmts_BQUOT of (
        Token.t (* "`" *) * statements * Token.t (* "`" *)
    )
]

and compound_statement = (
    Token.t (* "{" *)
  * statements2 option
  * Token.t (* "}" *)
)

and concatenation = (
    anon_choice_prim_exp_65e2c2e
  * (concat (*tok*) * anon_choice_prim_exp_65e2c2e) list (* one or more *)
  * (concat (*tok*) * Token.t (* "$" *)) option
)

and do_group = (
    Token.t (* "do" *)
  * statements2 option
  * Token.t (* "done" *)
)

and elif_clause = (
    Token.t (* "elif" *)
  * terminated_statement
  * Token.t (* "then" *)
  * statements2 option
)

and else_clause = (Token.t (* "else" *) * statements2 option)

and expansion = (
    Token.t (* "${" *)
  * [ `HASH of Token.t (* "#" *) | `BANG of Token.t (* "!" *) ] option
  * [
        `Var_name_EQ_opt_choice_conc of (
            variable_name (*tok*)
          * Token.t (* "=" *)
          * literal option
        )
      | `Choice_subs_opt_tok_prec_p1_slash_opt_regex_rep_choice_choice_conc of (
            [
                `Subs of subscript
              | `Choice_semg_meta of simple_variable_name
              | `Choice_STAR of special_variable_name
            ]
          * (tok_prec_p1_slash (*tok*) * regex (*tok*) option) option
          * [
                `Choice_conc of literal
              | `COLON of Token.t (* ":" *)
              | `COLONQMARK of Token.t (* ":?" *)
              | `EQ of Token.t (* "=" *)
              | `COLONDASH of Token.t (* ":-" *)
              | `PERC of Token.t (* "%" *)
              | `DASH of Token.t (* "-" *)
              | `HASH of Token.t (* "#" *)
            ]
              list (* zero or more *)
        )
    ]
      option
  * Token.t (* "}" *)
)

and expression = [
    `Choice_conc of literal
  | `Un_exp of (
        [ `BANG of Token.t (* "!" *) | `Test_op of test_operator (*tok*) ]
      * expression
    )
  | `Tern_exp of (
        expression * Token.t (* "?" *) * expression * Token.t (* ":" *)
      * expression
    )
  | `Bin_exp of binary_expression
  | `Post_exp of (
        expression
      * [ `PLUSPLUS of Token.t (* "++" *) | `DASHDASH of Token.t (* "--" *) ]
    )
  | `Paren_exp of (Token.t (* "(" *) * expression * Token.t (* ")" *))
]

and file_redirect = (
    file_descriptor (*tok*) option
  * [
        `LT of Token.t (* "<" *)
      | `GT of Token.t (* ">" *)
      | `GTGT of Token.t (* ">>" *)
      | `AMPGT of Token.t (* "&>" *)
      | `AMPGTGT of Token.t (* "&>>" *)
      | `LTAMP of Token.t (* "<&" *)
      | `GTAMP of Token.t (* ">&" *)
      | `GTBAR of Token.t (* ">|" *)
    ]
  * literal
)

and heredoc_body = [
    `Simple_here_body of simple_heredoc_body (*tok*)
  | `Here_body_begin_rep_choice_expa_here_body_end of (
        heredoc_body_beginning (*tok*)
      * [
            `Expa of expansion
          | `Simple_expa of simple_expansion
          | `Cmd_subs of command_substitution
          | `Here_body_middle of heredoc_body_middle (*tok*)
        ]
          list (* zero or more *)
      * heredoc_body_end (*tok*)
    )
]

and herestring_redirect = (Token.t (* "<<<" *) * literal)

and last_case_item = (
    literal
  * (Token.t (* "|" *) * literal) list (* zero or more *)
  * Token.t (* ")" *)
  * program
  * Token.t (* ";;" *) option
)

and literal = [
    `Conc of concatenation
  | `Choice_semg_deep_exp of primary_expression
  | `Rep1_spec_char of special_character (*tok*) list (* one or more *)
]

and primary_expression = [
    `Semg_deep_exp of (Token.t (* "<..." *) * literal * Token.t (* "...>" *))
  | `Choice_word of [
        `Word of word (*tok*)
      | `Str of string_
      | `Raw_str of raw_string (*tok*)
      | `Ansii_c_str of ansii_c_string (*tok*)
      | `Expa of expansion
      | `Simple_expa of simple_expansion
      | `Str_expa of string_expansion
      | `Cmd_subs of command_substitution
      | `Proc_subs of process_substitution
    ]
]

and process_substitution = (
    [ `LTLPAR of Token.t (* "<(" *) | `GTLPAR of Token.t (* ">(" *) ]
  * statements
  * Token.t (* ")" *)
)

and program = statements option

and statement = [
    `Redi_stmt of (
        statement
      * [
            `File_redi of file_redirect
          | `Here_redi_a9657de of heredoc_redirect
          | `Here_redi_7d3292d of herestring_redirect
        ]
          list (* one or more *)
    )
  | `Var_assign of variable_assignment
  | `Cmd of command
  | `Decl_cmd of (
        [
            `Decl of Token.t (* "declare" *)
          | `Type of Token.t (* "typeset" *)
          | `Export of Token.t (* "export" *)
          | `Read of Token.t (* "readonly" *)
          | `Local of Token.t (* "local" *)
        ]
      * [
            `Choice_conc of literal
          | `Choice_semg_meta of simple_variable_name
          | `Var_assign of variable_assignment
        ]
          list (* zero or more *)
    )
  | `Unset_cmd of (
        [
            `Unset of Token.t (* "unset" *)
          | `Unse of Token.t (* "unsetenv" *)
        ]
      * [
            `Choice_conc of literal
          | `Choice_semg_meta of simple_variable_name
        ]
          list (* zero or more *)
    )
  | `Test_cmd of test_command
  | `Nega_cmd of (
        Token.t (* "!" *)
      * [ `Cmd of command | `Test_cmd of test_command | `Subs of subshell ]
    )
  | `For_stmt of (
        [ `For of Token.t (* "for" *) | `Select of Token.t (* "select" *) ]
      * simple_variable_name
      * (Token.t (* "in" *) * literal list (* one or more *)) option
      * terminator
      * do_group
    )
  | `C_style_for_stmt of (
        Token.t (* "for" *)
      * Token.t (* "((" *)
      * expression option
      * terminator
      * expression option
      * terminator
      * expression option
      * Token.t (* "))" *)
      * Token.t (* ";" *) option
      * [ `Do_group of do_group | `Comp_stmt of compound_statement ]
    )
  | `While_stmt of (Token.t (* "while" *) * terminated_statement * do_group)
  | `If_stmt of (
        Token.t (* "if" *)
      * terminated_statement
      * Token.t (* "then" *)
      * statements2 option
      * elif_clause list (* zero or more *)
      * else_clause option
      * Token.t (* "fi" *)
    )
  | `Case_stmt of (
        Token.t (* "case" *)
      * literal
      * terminator option
      * Token.t (* "in" *)
      * terminator
      * (case_item list (* zero or more *) * last_case_item) option
      * Token.t (* "esac" *)
    )
  | `Pipe of (
        statement
      * [ `BAR of Token.t (* "|" *) | `BARAMP of Token.t (* "|&" *) ]
      * statement
    )
  | `List of (
        statement
      * [ `AMPAMP of Token.t (* "&&" *) | `BARBAR of Token.t (* "||" *) ]
      * statement
    )
  | `Subs of subshell
  | `Comp_stmt of compound_statement
  | `Func_defi of (
        [
            `Func_exte_word_opt_LPAR_RPAR of (
                Token.t (* "function" *)
              * extended_word
              * (Token.t (* "(" *) * Token.t (* ")" *)) option
            )
          | `Word_LPAR_RPAR of (
                word (*tok*) * Token.t (* "(" *) * Token.t (* ")" *)
            )
        ]
      * [
            `Comp_stmt of compound_statement
          | `Subs of subshell
          | `Test_cmd of test_command
        ]
    )
]

and statements = (
    anon_stmt_opt_LF_here_body_term_3efa649 list (* zero or more *)
  * statement
  * (Token.t (* "\n" *) * heredoc_body) option
  * terminator option
)

and statements2 =
  anon_stmt_opt_LF_here_body_term_3efa649 list (* one or more *)

and string_ = (
    Token.t (* "\"" *)
  * (
        [
            `Opt_DOLLAR_str_content of (
                Token.t (* "$" *) option
              * string_content (*tok*)
            )
          | `Expa of expansion
          | `Simple_expa of simple_expansion
          | `Cmd_subs of command_substitution
        ]
      * concat (*tok*) option
    )
      list (* zero or more *)
  * Token.t (* "$" *) option
  * Token.t (* "\"" *)
)

and string_expansion = (
    Token.t (* "$" *)
  * [ `Str of string_ | `Raw_str of raw_string (*tok*) ]
)

and subscript = (
    variable_name (*tok*)
  * Token.t (* "[" *)
  * literal
  * concat (*tok*) option
  * Token.t (* "]" *)
  * concat (*tok*) option
)

and subshell = (Token.t (* "(" *) * statements * Token.t (* ")" *))

and terminated_statement = (statement * terminator)

and test_command = [
    `LBRACK_exp_RBRACK of (
        Token.t (* "[" *) * expression * Token.t (* "]" *)
    )
  | `LBRACKLBRACK_exp_RBRACKRBRACK of (
        Token.t (* "[[" *) * expression * Token.t (* "]]" *)
    )
  | `LPARLPAR_exp_RPARRPAR of (
        Token.t (* "((" *) * expression * Token.t (* "))" *)
    )
]

and variable_assignment = [
    `Choice_semg_meta_eq_choice_choice_conc of (
        [
            `Semg_meta_eq of semgrep_metavar_eq (*tok*)
          | `Semg_meta_pluseq of semgrep_metavar_pluseq (*tok*)
        ]
      * anon_choice_lit_bbf16c7
    )
  | `Choice_var_name_choice_EQ_choice_choice_conc of (
        [ `Var_name of variable_name (*tok*) | `Subs of subscript ]
      * [ `EQ of Token.t (* "=" *) | `PLUSEQ of Token.t (* "+=" *) ]
      * anon_choice_lit_bbf16c7
    )
]

type comment (* inlined *) = Token.t

type array_ (* inlined *) = (
    Token.t (* "(" *)
  * literal list (* zero or more *)
  * Token.t (* ")" *)
)

type c_style_for_statement (* inlined *) = (
    Token.t (* "for" *)
  * Token.t (* "((" *)
  * expression option
  * terminator
  * expression option
  * terminator
  * expression option
  * Token.t (* "))" *)
  * Token.t (* ";" *) option
  * [ `Do_group of do_group | `Comp_stmt of compound_statement ]
)

type case_statement (* inlined *) = (
    Token.t (* "case" *)
  * literal
  * terminator option
  * Token.t (* "in" *)
  * terminator
  * (case_item list (* zero or more *) * last_case_item) option
  * Token.t (* "esac" *)
)

type declaration_command (* inlined *) = (
    [
        `Decl of Token.t (* "declare" *)
      | `Type of Token.t (* "typeset" *)
      | `Export of Token.t (* "export" *)
      | `Read of Token.t (* "readonly" *)
      | `Local of Token.t (* "local" *)
    ]
  * [
        `Choice_conc of literal
      | `Choice_semg_meta of simple_variable_name
      | `Var_assign of variable_assignment
    ]
      list (* zero or more *)
)

type for_statement (* inlined *) = (
    [ `For of Token.t (* "for" *) | `Select of Token.t (* "select" *) ]
  * simple_variable_name
  * (Token.t (* "in" *) * literal list (* one or more *)) option
  * terminator
  * do_group
)

type function_definition (* inlined *) = (
    [
        `Func_exte_word_opt_LPAR_RPAR of (
            Token.t (* "function" *)
          * extended_word
          * (Token.t (* "(" *) * Token.t (* ")" *)) option
        )
      | `Word_LPAR_RPAR of (
            word (*tok*) * Token.t (* "(" *) * Token.t (* ")" *)
        )
    ]
  * [
        `Comp_stmt of compound_statement
      | `Subs of subshell
      | `Test_cmd of test_command
    ]
)

type if_statement (* inlined *) = (
    Token.t (* "if" *)
  * terminated_statement
  * Token.t (* "then" *)
  * statements2 option
  * elif_clause list (* zero or more *)
  * else_clause option
  * Token.t (* "fi" *)
)

type list_ (* inlined *) = (
    statement
  * [ `AMPAMP of Token.t (* "&&" *) | `BARBAR of Token.t (* "||" *) ]
  * statement
)

type negated_command (* inlined *) = (
    Token.t (* "!" *)
  * [ `Cmd of command | `Test_cmd of test_command | `Subs of subshell ]
)

type parenthesized_expression (* inlined *) = (
    Token.t (* "(" *) * expression * Token.t (* ")" *)
)

type pipeline (* inlined *) = (
    statement
  * [ `BAR of Token.t (* "|" *) | `BARAMP of Token.t (* "|&" *) ]
  * statement
)

type postfix_expression (* inlined *) = (
    expression
  * [ `PLUSPLUS of Token.t (* "++" *) | `DASHDASH of Token.t (* "--" *) ]
)

type redirected_statement (* inlined *) = (
    statement
  * [
        `File_redi of file_redirect
      | `Here_redi_a9657de of heredoc_redirect
      | `Here_redi_7d3292d of herestring_redirect
    ]
      list (* one or more *)
)

type semgrep_deep_expression (* inlined *) = (
    Token.t (* "<..." *) * literal * Token.t (* "...>" *)
)

type ternary_expression (* inlined *) = (
    expression * Token.t (* "?" *) * expression * Token.t (* ":" *)
  * expression
)

type unary_expression (* inlined *) = (
    [ `BANG of Token.t (* "!" *) | `Test_op of test_operator (*tok*) ]
  * expression
)

type unset_command (* inlined *) = (
    [ `Unset of Token.t (* "unset" *) | `Unse of Token.t (* "unsetenv" *) ]
  * [ `Choice_conc of literal | `Choice_semg_meta of simple_variable_name ]
      list (* zero or more *)
)

type while_statement (* inlined *) = (
    Token.t (* "while" *) * terminated_statement * do_group
)

type extra = [ `Comment of Loc.t * comment ]

type extras = extra list
