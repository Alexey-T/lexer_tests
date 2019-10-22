%run Relation Any Any Any;run2 Relation Any Any Any Any Any;step Relation Any Integer Integer Any Any Any;match_list Relation Any Any Any;parse_id Relation Any Any Any;parse_number Relation Any Any Any;parse_string Relation Any Any Any;parse_double_string Relation Any Any Any;match_whitespace Relation Any Any Any;navigate_whitespace Relation Any Any Any Any Any;navigate_ws2 Relation String Integer Integer Any Any Any;navigate_comment Relation Any Any Any Any Any;navigate_until_asterisk Relation Any Any Any Any Any;match_string Relation Any Any Any Any;match_until Relation Any Any Any Any;match_any Relation Any Any Any Any;match_one Relation Any Any Any Any;match_some Relation Any Any Any Any;match_rel2 Relation Any Any Any Any;whitespace Relation Any;endline Relation Any;quote Relation Any;double_quote Relation Any;slash Relation Any;single_quote Relation Any;letter Relation Any;digit Relation Any;Info FunctorObject;Token FunctorObject;Cons FunctorObject;Tuple FunctorObject
:- style_check([-singleton,-no_effect]), ensure_loaded("core.pl").
lexer0(Env0,_c) :- env_call(Env0,"letter",[Env0,_c]);eq(_c,"_").
lexer_digit(Env0,_c) :- obj_call2(Env0,"string","lessOrEqual",[_c,"9"]),obj_call2(Env0,"string","lessOrEqual",["0",_c]).
lexer_letter(Env0,_c) :- obj_call2(Env0,"string","lessOrEqual",[_c,"z"]),obj_call2(Env0,"string","lessOrEqual",["a",_c]);obj_call2(Env0,"string","lessOrEqual",[_c,"Z"]),obj_call2(Env0,"string","lessOrEqual",["A",_c]).
lexer_single_quote(Env0,_c) :- obj_call2(Env0,"string","code",[_c,39]).
lexer_slash(Env0,_c) :- obj_call2(Env0,"string","code",[_c,92]).
lexer_double_quote(Env0,_c) :- eq(_c,"\"").
lexer_quote(Env0,_c) :- env_call(Env0,"single_quote",[Env0,_c]);env_call(Env0,"double_quote",[Env0,_c]).
lexer_endline(Env0,_c) :- eq(_c,"\n").
lexer_whitespace(Env0,_c) :- eq(_c," ");eq(_c,"\t");eq(_c,"\n").
lexer_match_rel2(Env0,_s,_i,_i2,_f) :- obj_call2(Env0,"string","at",[_s,_i,_c]),closure_call(Env0,_f,[_c]),fd_add(_i,1,T1),env_call(Env0,"match_rel2",[Env0,_s,T1,_i2,_f]);eq(_i,_i2).
lexer_match_some(Env0,_s,_i,_i2,_f) :- obj_call2(Env0,"string","at",[_s,_i,_c]),closure_call(Env0,_f,[_c]),fd_add(_i,1,T2),env_call(Env0,"match_rel2",[Env0,_s,T2,_i2,_f]).
lexer_match_one(Env0,_s,_i,_i2,_f) :- fd_add(_i,1,T3),eq(_i2,T3),obj_call2(Env0,"string","at",[_s,_i,_c]),closure_call(Env0,_f,[_c]).
lexer_match_any(Env0,_s,_i,_i2,_f) :- obj_call2(Env0,"string","at",[_s,_i,_c]),closure_call(Env0,_f,[_c]),fd_add(_i,1,T4),env_call(Env0,"match_any",[Env0,_s,T4,_i2,_f]);eq(_i,_i2).
lexer_match_until(Env0,_s,_i,_i2,_f) :- obj_call2(Env0,"string","at",[_s,_i,_c]),closure_call(Env0,_f,[_c]),eq(_i,_i2);fd_add(_i,1,T5),env_call(Env0,"match_until",[Env0,_s,T5,_i2,_f]).
lexer_match_string(Env0,_s,_i,_i2,_str) :- obj_call2(Env0,"string","size",[_str,_size]),dynamic_add(_i,_size,T6),eq(_i2,T6),obj_call2(Env0,"string","slice",[_s,_i,_i2,_str]).
lexer_navigate_until_asterisk(Env0,_s,_i,_i2,_info,_info2) :- eq(_info,fcInfo(_line,_col)),obj_call2(Env0,"string","at",[_s,_i,_c]),(((eq(_c,"*")),eq(_i,_i2),eq(_info2,_info));((((eq(_c,"\n")),fd_add(_line,1,T7),eq(_info1,fcInfo(T7,1)));((eq(_c,"\t")),fd_add(_col,4,T8),eq(_info1,fcInfo(_line,T8)));(fd_add(_col,1,T9),eq(_info1,fcInfo(_line,T9)))),fd_add(_i,1,T10),env_call(Env0,"navigate_until_asterisk",[Env0,_s,T10,_i2,_info1,_info2]))).
lexer_navigate_comment(Env0,_s,_i,_i2,_info,_info2) :- (((env_call(Env0,"match_string",[Env0,_s,_i,_x,"//"])),eq(_info2,_info),(((obj_call2(Env0,"string","findIndex",[_s,"\n",_x,_i2])),true);(obj_call2(Env0,"string","size",[_s,T11]),eq(_i2,T11))));(env_call(Env0,"match_string",[Env0,_s,_i,_x,"/*"]),env_call(Env0,"navigate_until_asterisk",[Env0,_s,_x,_y,_info,_info2]),env_call(Env0,"match_string",[Env0,_s,_y,_i2,"*/"]))).
lexer_navigate_ws2(Env0,_s,_i,_i2,_line,_col,_info2) :- (((obj_call2(Env0,"string","at",[_s,_i,_c])),(((eq(_c,"\n")),fd_add(_line,1,T13),fd_add(_i,1,T12),env_call(Env0,"navigate_ws2",[Env0,_s,T12,_i2,T13,1,_info2]));((eq(_c,"\t")),fd_add(_col,4,T15),fd_add(_i,1,T14),env_call(Env0,"navigate_ws2",[Env0,_s,T14,_i2,_line,T15,_info2]));(env_call(Env0,"whitespace",[Env0,_c]),fd_add(_col,1,T17),fd_add(_i,1,T16),env_call(Env0,"navigate_ws2",[Env0,_s,T16,_i2,_line,T17,_info2]))));(eq(_i2,_i),eq(_info2,fcInfo(_line,_col)))).
lexer_navigate_whitespace(Env0,_s,_i,_i2,_info,_info2) :- obj_call2(Env0,"string","at",[_s,_i,_c]),env_call(Env0,"whitespace",[Env0,_c]),eq(_info,fcInfo(_line,_col)),(((eq(_c,"\n")),fd_add(_line,1,T19),fd_add(_i,1,T18),env_call(Env0,"navigate_ws2",[Env0,_s,T18,_i2,T19,1,_info2]));(fd_add(_i,1,T20),env_call(Env0,"navigate_ws2",[Env0,_s,T20,_i2,_line,_col,_info2]))).
lexer_match_whitespace(Env0,_s,_i,_i2) :- env_get(Env0,"whitespace",T21),env_call(Env0,"match_some",[Env0,_s,_i,_i2,closure(T21,Env0)]).
lexer_parse_double_string(Env0,_s,_i,_i2) :- obj_call2(Env0,"string","at",[_s,_i,_c]),(((env_call(Env0,"double_quote",[Env0,_c])),eq(_i,_i2));((env_call(Env0,"slash",[Env0,_c])),fd_add(_i,2,T22),env_call(Env0,"parse_double_string",[Env0,_s,T22,_i2]));(fd_add(_i,1,T23),env_call(Env0,"parse_double_string",[Env0,_s,T23,_i2]))).
lexer_parse_string(Env0,_s,_i,_i2) :- fd_add(_i,1,T24),eq(_j,T24),obj_call2(Env0,"string","at",[_s,_i,_c]),(((env_call(Env0,"double_quote",[Env0,_c])),env_call(Env0,"parse_double_string",[Env0,_s,_j,_j2]),fd_add(_j2,1,T25),eq(_i2,T25));(env_call(Env0,"single_quote",[Env0,_c]),env_get(Env0,"single_quote",T26),env_call(Env0,"match_until",[Env0,_s,_j,_j2,closure(T26,Env0)]),fd_add(_j2,1,T27),eq(_i2,T27))).
lexer_parse_number(Env0,_s,_i,_i2) :- env_get(Env0,"digit",T28),env_call(Env0,"match_some",[Env0,_s,_i,_x1,closure(T28,Env0)]),env_call(Env0,"match_string",[Env0,_s,_x1,_x2,"."]),env_get(Env0,"digit",T29),env_call(Env0,"match_some",[Env0,_s,_x2,_i2,closure(T29,Env0)]);env_get(Env0,"digit",T30),env_call(Env0,"match_some",[Env0,_s,_i,_i2,closure(T30,Env0)]).
lexer_parse_id(Env0,_s,_i,_i2) :- eq(_p,closure(lexer0,Env0)),env_call(Env0,"match_some",[Env0,_s,_i,_x,_p]),env_get(Env0,"digit",T31),env_call(Env0,"match_any",[Env0,_s,_x,_i2,closure(T31,Env0)]).
lexer_match_list(Env0,_l,_s,_z) :- eq(_l, '[|]'(_head,_tail)),((eq(_head,_s))->(eq(_z,_s));(env_call(Env0,"match_list",[Env0,_tail,_s,_z]))).
lexer_step(Env0,_s,_i,_j,_z,_info,_info2) :- (((env_call(Env0,"navigate_whitespace",[Env0,_s,_i,_j,_info,_info2])),eq(_z,"whitespace"));((env_call(Env0,"navigate_comment",[Env0,_s,_i,_j,_info,_info2])),eq(_z,"comment"));(obj_call2(Env0,"string","at",[_s,_i,_c]),fd_add(_i,1,T32),eq(_k,T32),(((env_call(Env0,"match_list",[Env0,'[|]'("=",'[|]'(";",'[|]'("(",'[|]'(")",'[|]'("{",'[|]'("}",'[|]'(".",'[|]'(",",[])))))))),_c,_z])),fd_add(_i,1,T33),eq(_j,T33));((env_call(Env0,"letter",[Env0,_c])),env_call(Env0,"match_string",[Env0,_s,_i,_j,"rel"]),eq(_z,"rel");env_call(Env0,"match_string",[Env0,_s,_i,_j,"and"]),eq(_z,"connective");env_call(Env0,"match_string",[Env0,_s,_i,_j,"or"]),eq(_z,"connective");env_call(Env0,"match_string",[Env0,_s,_i,_j,"true"]),eq(_z,"true");env_call(Env0,"match_string",[Env0,_s,_i,_j,"false"]),eq(_z,"false");env_call(Env0,"match_string",[Env0,_s,_i,_j,"if"]),eq(_z,"if");env_call(Env0,"match_string",[Env0,_s,_i,_j,"choose"]),eq(_z,"choose");env_call(Env0,"match_string",[Env0,_s,_i,_j,"elseif"]),eq(_z,"elseif");env_call(Env0,"match_string",[Env0,_s,_i,_j,"else"]),eq(_z,"else");env_call(Env0,"match_string",[Env0,_s,_i,_j,"once"]),eq(_z,"once"));(env_call(Env0,"match_string",[Env0,_s,_i,_j,"!="]),eq(_z,"!=");env_call(Env0,"match_string",[Env0,_s,_i,_j,"::"]),eq(_z,"::");env_call(Env0,"parse_id",[Env0,_s,_i,_j]),eq(_z,"id");env_call(Env0,"parse_number",[Env0,_s,_i,_j]),eq(_z,"number");env_call(Env0,"parse_string",[Env0,_s,_i,_j]),eq(_z,"string");env_call(Env0,"match_string",[Env0,_s,_i,_j,"<="]),eq(_z,"<=");env_call(Env0,"match_string",[Env0,_s,_i,_j,">="]),eq(_z,">=");env_call(Env0,"match_list",[Env0,'[|]'("+",'[|]'("-",'[|]'("<",'[|]'(">",'[|]'("[",'[|]'("]",'[|]'("/",'[|]'("*",[])))))))),_c,_z]),eq(_j,_k);env_get(Env0,"single_quote",T34),env_call(Env0,"match_one",[Env0,_s,_i,_j,closure(T34,Env0)]),eq(_z,"single_quote");env_get(Env0,"double_quote",T35),env_call(Env0,"match_one",[Env0,_s,_i,_j,closure(T35,Env0)]),eq(_z,"double_quote");eq(_info,fcInfo(_line,_col)),obj_call2(Env0,"math","integerToString",[_line,T40]),string_concat("(line ",T40,T39),dynamic_add(T39,", col ",T38),obj_call2(Env0,"math","integerToString",[_col,T41]),dynamic_add(T38,T41,T37),dynamic_add(T37,") cannot understand character",T36),obj_call2(Env0,"logic","throw",[T36]))),eq(_info,fcInfo(_line,_col)),fd_add(_col,_j,T43),fd_sub(T43,_i,T42),eq(_info2,fcInfo(_line,T42)))).
lexer_run2(Env0,_s,_start,_l,_info,_info2) :- (((true),env_call(Env0,"step",[Env0,_s,_start,_x,_type,_info,_info1]),env_call(Env0,"run2",[Env0,_s,_x,_l0,_info1,_info2]),(((neq(_type,"comment")),obj_call2(Env0,"string","slice",[_s,_start,_x,_lexeme]),eq(_tk,fcToken(_lexeme,_type,_info)),obj_call2(Env0,"list","push",[_l0,_tk,_l]));(eq(_l,_l0))));(eq(_l, '[|]'(fcToken("EOF","EOF",_),[])),obj_call2(Env0,"string","size",[_s,_z]),eq(_z,_start),eq(_info2,_info))).
lexer_run(Env0,_s,_start,_l) :- env_call(Env0,"run2",[Env0,_s,_start,_l,fcInfo(1,1),_info2]).
lexer_env(X) :- new(X0),set(X0,"run",lexer_run,X1),set(X1,"run2",lexer_run2,X2),set(X2,"step",lexer_step,X3),set(X3,"match_list",lexer_match_list,X4),set(X4,"parse_id",lexer_parse_id,X5),set(X5,"parse_number",lexer_parse_number,X6),set(X6,"parse_string",lexer_parse_string,X7),set(X7,"parse_double_string",lexer_parse_double_string,X8),set(X8,"match_whitespace",lexer_match_whitespace,X9),set(X9,"navigate_whitespace",lexer_navigate_whitespace,X10),set(X10,"navigate_ws2",lexer_navigate_ws2,X11),set(X11,"navigate_comment",lexer_navigate_comment,X12),set(X12,"navigate_until_asterisk",lexer_navigate_until_asterisk,X13),set(X13,"match_string",lexer_match_string,X14),set(X14,"match_until",lexer_match_until,X15),set(X15,"match_any",lexer_match_any,X16),set(X16,"match_one",lexer_match_one,X17),set(X17,"match_some",lexer_match_some,X18),set(X18,"match_rel2",lexer_match_rel2,X19),set(X19,"whitespace",lexer_whitespace,X20),set(X20,"endline",lexer_endline,X21),set(X21,"quote",lexer_quote,X22),set(X22,"double_quote",lexer_double_quote,X23),set(X23,"slash",lexer_slash,X24),set(X24,"single_quote",lexer_single_quote,X25),set(X25,"letter",lexer_letter,X26),set(X26,"digit",lexer_digit,X27),set(X27,"Info",lexer_Info,X28),set(X28,"Token",lexer_Token,X29),set(X29,"Cons",lexer_Cons,X30),set(X30,"Tuple",lexer_Tuple,X31),table_env(Y31),set(X31,"table",Y31,X32),list_env(Y32),set(X32,"list",Y32,X33),string_env(Y33),set(X33,"string",Y33,X34),math_env(Y34),set(X34,"math",Y34,X35),io_env(Y35),set(X35,"io",Y35,X36),logic_env(Y36),set(X36,"logic",Y36,X37),set(X37,"Some",lexer_Some,X38),set(X38,"None",lexer_None,X39),set(X39,"Left",lexer_Left,X40),set(X40,"Right",lexer_Right,X41),X=X41.