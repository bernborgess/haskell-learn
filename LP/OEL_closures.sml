(*
 OEL : Our Expression Language
*)


datatype expr =
  IConst of int
  | BConst of bool
  | Prim2 of string * expr * expr
  | Prim1 of string * expr
  | Ite of expr * expr * expr
  | Var of string
  | Let of string * expr * expr
  (* Declaring functions
    (f, x, fBody, fDeclEnv)
  *)
  | LetFun of string * string * expr * expr
  (* For evaluating closure values *)
  | Call of expr * expr;



val e0 = LetFun("f","x",Prim2("+",Var "x",Var"a"),Call(Var "f",IConst 1));


datatype value = 
  Int of int
 | Closure of string * string* expr * (string*value) list;

exception EvalError;
exception PrimError;
exception FreeVar;

fun lookup [] id = raise FreeVar
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;

fun intToBool 1 = true
  | intToBool 0 = false
  | intToBool _ = raise Match;

fun boolToInt true = 1
  | boolToInt false = 0;



fun eval (e:expr) (st: (string * value) list) : int =
    case e of
        (IConst i) => i
      | (BConst b) => boolToInt b
      | (Var v) =>
        let val vv = lookup st v in
            case vv of
                (Int i) => i
              | _ => raise EvalError
        end
      | (Prim2(f, e1, e2)) =>
        let
            val v1 = (eval e1 st);
            val v2 = (eval e2 st) in
        case f of
            ("+") => v1 + v2
          | ("-") => v1 - v2
          | ("*") => v1 * v2
          | ("/") => v1 div v2
          | ("=") => if v1 = v2 then 1 else 0
          | ("<") => if v1 < v2 then 1 else 0
          | ("and") => boolToInt ((intToBool v1) andalso (intToBool v2))
          | ("or") => boolToInt ((intToBool v1) orelse (intToBool v2))
          | _ => raise PrimError
        end
      | (Prim1("not", e1)) => boolToInt (not (intToBool (eval e1 st)))
      | (Ite(c, t, e)) => if (intToBool (eval c st)) then eval t st else eval e st
      | (Let(x, e1, e2)) => eval e2 ((x, Int (eval e1 st))::st)
      (* Declaring a function generates a closure with the current
      state saved as the declaration environment *)
      | (LetFun(f, x, e1, e2)) => eval e2 ((f, Closure(f, x, e1, st))::st)
      (* We force the language to be first-order by allowying only
      named functions to be called and only accepting non-functional
      arguments *)
      | (Call(Var f, e)) =>
        let val fv = (lookup st f) in
            case fv of
                (Closure(f, x, e1, fSt)) =>
                let
                    (* the function input is evaluated in the current state *)
                    val ev = Int(eval e st);
                    (* the function body is evaluated in the scope
                    saved in the closure. *)
                    val st' = (x, ev) :: (f, fv) :: fSt
                in
                    eval e1 st'
                end
             | _ => raise EvalError
        end
      | _ => raise Match;

fun isIn x s =
    case s of
        [] => false
      | (h::t) => if x = h then true else isIn x t;

fun union s1 s2 =
    case s1 of
        [] => s2
      | (h::t) => if isIn h s2 then union t s2 else union t (h::s2);

fun diff s1 s2 =
    case s1 of
        [] => []
      | (h::t) => if isIn h s2 then diff t s2 else h::(diff t s2);

fun freeVars e : string list =
    case e of
        IConst _ => []
      | BConst _ =>  []
      | (Var v) => [v]
      | (Prim2(_, e1, e2)) => union (freeVars e1) (freeVars e2)
      | (Prim1(_, e1)) => freeVars e1
      | (Ite(c, t, e)) => union (union (freeVars c) (freeVars t)) (freeVars e)
      | (Let(v, e1, e2)) =>
        let val v1 = freeVars e1;
            val v2 = freeVars e2
        in
            union v1 (diff v2 [v]) (* let binds x in e2 but not in e1 *)
        end
      | (LetFun(f, x, e1, e2)) =>
        let val v1 = freeVars e1;
            val v2 = freeVars e2
        in
            (* letFun binds f in e2 and x in e1 *)
            union (diff v1 [x]) (diff v2 [f])
        end
      | (Call(Var f, e)) => freeVars e
      | _ => raise Match;

fun closed e = (freeVars e = []);


exception NonClosed;


fun run e =
    if closed e then
        eval e []
    else
        raise NonClosed;


(* RODAR ISSO NO SMLNJ  (existe a funcao union?) *)

(* e0;

run e0; *)


val e2 = Let("a",IConst 5,
    LetFun("f","x",Prim2("+",Var"x",Var"a"),
        Let("a", IConst 3,
            Call(Var"f",IConst 1)
            )
        )
    );

(* closed e2; *)
print "\nRun e2: ";
run e2;
(* => 6 ?? *)



(* RECURSIVA *)

fun fact n = LetFun("fact","x",
    Ite(Prim2("=",Var"x",IConst 0),
        IConst 1,
        Prim2("*",
            Var "x",
            Call(Var "fact",Prim2("-",Var"x",IConst 1))
            )
        ),
    Call(Var"fact",IConst n)
    );


(* fact 0; *)

print "\nrun (fact 10): ";
run (fact 10);
