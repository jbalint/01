(* CConv example:
+ https://github.com/c-cube/cconv
+ http://cedeela.fr/universal-serialization-and-deserialization.html

Install cconv:
$ ./configure --enable-sexp
$ ocamlfind remove cconv # if re-installing
$ make install PREFIX=/home/jbalint
$ ocamlfind list | grep cconv
cconv               (version: 0.2)
cconv.psexp         (version: 0.2)
cconv.sexp          (version: 0.2)

Compilation:
$ ocamlfind ocamlopt -c -package cconv cconv_example.ml 
$ ocamlfind ocamlopt -o cconv_example -linkpkg -package cconv cconv_example.cmx

 *)

(*
#require "cconv";;
#require "cconv.psexp";;
#require "cconv.sexp";;
 *)

type term =
	Const of string
  | Var of string
  | Pred of string * (term * term)

type horn_clause = Horn of term * term list

type index_entry = TermEntry of term | HornEntry of horn_clause

(* Char.code 'a' *)
let lowercasep c = c >= 'a'

let encode = CConv.Encode.(sum_fix
    (fun self -> {sum_emit=fun into t -> match t with
        | Const s -> s, []
		| Var s -> s, [] (* or with nested sexp: "var", [string.emit into v] *)
		| Pred (name, (t1, t2)) -> "pred", [string.emit into name;
											self.emit into t1;
											self.emit into t2]}))

let decode = CConv.Decode.(sum_fix
    (fun self -> {
	   sum_accept=fun src name args -> match name, args with
									   | name, [] when lowercasep name.[0] -> Const name
									   | name, [] -> Var name (* ??? (apply src string name) *)
									   | "pred", [pname; a1; a2] ->
										  let arg1 = apply src self a1 in
										  let arg2 = apply src self a2 in
										  Pred (apply src string pname, (arg1, arg2))
									   | _ -> CConv.report_error "unexpected"
	 }))

let test_terms = [Const "bstest:Socrates";
				  Const "\"abc\"";
				  Pred ("genls", (Const "document", Const "book"));
				  (* x1(x2(a, b), V) *)
				  Pred ("x1", (Pred ("x2", (Const "a", Const"b")), Var "V"));
				  Var "AbasicVAR"]

let _ = CConvSexp.encode encode (List.nth test_terms 1)

let serialized_test_terms = test_terms |> List.map @@ CConvSexp.to_string encode

let deserialized_test_terms = serialized_test_terms |> List.map @@ CConvSexp.of_string_exn decode

let _ = List.nth deserialized_test_terms 1
