%{
open Ast
open Utils
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> IDENT STRING
%token          EOF NEWLINE
%token          PLUS MINUS STAR SLASH MOD POW LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET AMPAND DOT
%token          EQ COMMA EQEQ NE GT GTE LT LTE COLON
%token          RETURN TRUE FALSE EXTERN NIL MUT
%token          TYPE TVOID TBOOL STRUCT TSTRING
%token          TINT TINT8 TINT16 TINT32 TINT64 TINT128
%token          TUINT TUINT8 TUINT16 TUINT32 TUINT64 TUINT128
%token          TFLOAT TFLOAT32 TFLOAT64

%left EQEQ NE GT GTE LT LTE
%left PLUS MINUS
%left STAR SLASH MOD
%right POW

%start <Program.t> start
%%

start:
  | s=statements EOF {
      Program.make ~modules:[
        {name="main"; block={statements=s; ty=None}};
      ] ()
    }

statements:
  | statement {[$1]}
  | statement newlines {[$1]}
  | statement newlines statements {$1 :: $3}

statement:
  | declaration_variable
  | declaration_struct
  | declaration_type        { $1 }
  | block                   { mk_stmt $startpos (`Block $1) }
  | RETURN expr=expr        { mk_stmt $startpos (`Return expr) }
  | e=expr_call             { mk_stmt $startpos (`Let { name="_"; ty=Resolver.TypeResolver.of_expr e; value=e; mut=false }) }
  | name=path EQ value=expr { mk_stmt $startpos (`Assign { name = String.concat "." name; value }) }

types:
  | TVOID   { mk_types $startpos (`TVoid ()) }
  | TBOOL   { mk_types $startpos (`TBool ()) }
  | TSTRING { mk_types $startpos (`TString ()) }
  | TINT    { mk_types $startpos (`TInt 32) }
  | TINT8   { mk_types $startpos (`TInt 8) }
  | TINT16  { mk_types $startpos (`TInt 16) }
  | TINT32  { mk_types $startpos (`TInt 32) }
  | TINT64  { mk_types $startpos (`TInt 64) }
  | TINT128 { mk_types $startpos (`TInt 128) }
  | TUINT   { mk_types $startpos (`TUint 32) }
  | TUINT8  { mk_types $startpos (`TUint 8) }
  | TUINT16 { mk_types $startpos (`TUint 16) }
  | TUINT32 { mk_types $startpos (`TUint 32) }
  | TUINT64 { mk_types $startpos (`TUint 64) }
  | TUINT128{ mk_types $startpos (`TUint 128) }
  | TFLOAT  { mk_types $startpos (`TFloat 64) }
  | TFLOAT32{ mk_types $startpos (`TFloat 32) }
  | TFLOAT64{ mk_types $startpos (`TFloat 64) }
  | LPAREN RPAREN ty=types { mk_types $startpos (`TFn {params=[]; ty; name=None}) }
  | LPAREN params=separated_nonempty_list(COMMA, types) RPAREN ty=types { mk_types $startpos (`TFn {params; ty; name=None}) }
  | IDENT { mk_types $startpos (`Named $1) }
  | ty=types STAR { mk_types $startpos (`Reference ty) }
  | type_array { $1 }

declaration_variable:
  | name=IDENT COLON EQ value=expr              { mk_stmt $startpos ( `Let { name; ty=Resolver.TypeResolver.of_expr value; value; mut=false } ) }
  | name=IDENT COLON ty=types EQ value=expr     { mk_stmt $startpos ( `Let { name; ty; value; mut=false } ) }
  | name=IDENT COLON MUT EQ value=expr     { mk_stmt $startpos ( `Let { name; ty=Resolver.TypeResolver.of_expr value; value; mut=true } ) }
  | name=IDENT COLON MUT ty=types     { mk_stmt $startpos ( `Let { name; ty; value=Resolver.TypeResolver.default_value ty; mut=true } ) }
  | name=IDENT COLON MUT ty=types EQ value=expr     { mk_stmt $startpos ( `Let { name; ty; value; mut=true } ) }

newlines:
  | NEWLINE+                   { () }

expr:
  | INT                       { mk_expr $startpos (`Int $1) }
  | FLOAT                     { mk_expr $startpos (`Float $1) }
  | TRUE                      { mk_expr $startpos (`Bool true)}
  | FALSE                     { mk_expr $startpos (`Bool false)}
  | s=STRING                  { mk_expr $startpos (`String s )}
  | LPAREN e = expr RPAREN    { e }
  | l = expr PLUS  r = expr   { mk_expr $startpos (`Add {l; r}) }
  | l = expr MINUS r = expr   { mk_expr $startpos (`Sub {l; r}) }
  | l = expr STAR  r = expr   { mk_expr $startpos (`Mul {l; r}) }
  | l = expr SLASH r = expr   { mk_expr $startpos (`Div {l; r}) }
  | l = expr MOD   r = expr   { mk_expr $startpos (`Mod {l; r}) }
  | l = expr POW   r = expr   { mk_expr $startpos (`Pow {l; r}) }
  | l = expr EQEQ   r = expr  { mk_expr $startpos (`Eq {l; r}) }
  | l = expr NE   r = expr    { mk_expr $startpos (`Ne {l; r}) }
  | l = expr LT   r = expr    { mk_expr $startpos (`Lt {l; r}) }
  | l = expr LTE   r = expr   { mk_expr $startpos (`Lte {l; r}) }
  | l = expr GT   r = expr    { mk_expr $startpos (`Gt {l; r}) }
  | l = expr GTE   r = expr   { mk_expr $startpos (`Gte {l; r}) }
  | array                     { $1 }
  | name=path                 { mk_expr $startpos (`Var {name=String.concat "." name; ty=None; value=None})}
  | expr_call                 { $1 }
  | expr_struct               { $1 }
  | EXTERN name=STRING        { mk_expr $startpos (`Extern name) }
  | AMPAND name=IDENT         { mk_expr $startpos (`Reference {name; ty=None; value=None}) }
  | STAR name=path            { mk_expr $startpos (`Dereference {name=String.concat "." name; ty=None; value=None}) }
  | declaration_function      { $1 }
  | NIL                       { mk_expr $startpos (`Nil ()) }

type_array:
  | LBRACKET RBRACKET ty=types { mk_types $startpos (`TArray {ty; size=None}) }
  | LBRACKET size=INT RBRACKET ty=types { mk_types $startpos (`TArray {ty; size=Some(size)}) }

array:
  | LBRACKET RBRACKET { mk_expr $startpos (`Array {ty=None; items=[]}) }
  | LBRACKET items=separated_nonempty_list(COMMA, expr) RBRACKET { mk_expr $startpos (`Array {ty=None; items}) }

declaration_function:
  | LPAREN RPAREN b=block     
      { mk_expr $startpos (`Fn { ty=None; params=[]; block=b }) }
  | LPAREN RPAREN ty=types b=block     
      { mk_expr $startpos (`Fn { ty=Some(ty); params=[]; block=b }) }
  | LPAREN p=separated_nonempty_list(COMMA, function_param) RPAREN b=block     
      { mk_expr $startpos (`Fn { ty=None; params=p; block=b }) }
  | LPAREN p=separated_nonempty_list(COMMA, function_param) RPAREN ty=types b=block     
      { mk_expr $startpos (`Fn { ty=Some(ty); params=p; block=b }) }

function_param:
  | n=IDENT ty=types    { let p: Param.t = {name=n; ty} in p }

expr_call:
  | name=IDENT LPAREN RPAREN                                                 { mk_expr $startpos (`Call { name; params=[]; fn=None }) }
  | name=IDENT LPAREN p=separated_nonempty_list(COMMA, expr) RPAREN          { mk_expr $startpos (`Call { name; params=p; fn=None }) }

block:
  | LBRACE option(newlines) RBRACE
    { let b: SBlock.t = SBlock.make ~statements:[] () in b }
  | LBRACE option(newlines) s=statements RBRACE
    { let b: SBlock.t = SBlock.make ~statements:s () in b}

declaration_type:
  | TYPE name=IDENT ty=types     { mk_stmt $startpos (`Type {name; ty}) }

declaration_struct:
  | STRUCT name=IDENT LBRACE option(newlines) RBRACE { mk_stmt $startpos (`Struct {fields=[]; name}) }
  | STRUCT name=IDENT LBRACE option(newlines) fields=struct_fields option(newlines) RBRACE { mk_stmt $startpos (`Struct {fields; name}) }
struct_fields:
  | struct_field      { [ $1 ] }
  | struct_fields option(newlines) struct_field      { $1 @ [ $3 ] }
struct_field:
  | ty=types name=IDENT COMMA     { let f:StructField.t = {name; ty} in f }

expr_struct:
  | LBRACE option(newlines) RBRACE         { mk_expr $startpos (`Struct {name=None; values=[]}) }
  | LBRACE option(newlines) values=struct_values option(newlines) RBRACE         { mk_expr $startpos (`Struct {name=None; values}) }
  | name=IDENT LBRACE option(newlines) RBRACE         { mk_expr $startpos (`Struct {name=Some name; values=[]}) }
  | name=IDENT LBRACE option(newlines) values=struct_values option(newlines) RBRACE         { mk_expr $startpos (`Struct {name=Some name; values}) }
struct_values:
  | struct_value                                    { [ $1 ] }
  | struct_values option(newlines) struct_value      { $1 @ [ $3 ] }
struct_value:
  | name=IDENT EQ value=expr option(COMMA)     { let v:StructValue.t = {name; value; ty=None} in v }

path:
  | id=IDENT                { [id] }
  | id=IDENT DOT p=path     { id::p }

