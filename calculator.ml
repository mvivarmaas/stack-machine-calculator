type exp = 
  | PLUS  of exp * exp  (* Plus *)
  | MINUS of exp * exp  (* Minus *)
  | MULT of exp * exp  (* Mult *)
  | DIV   of exp * exp  (* Div *)
  | SIN   of exp        (* Sin *)
  | COS   of exp        (* Cos *)
  | EXP   of exp        (* Exp *)
  | FLOAT of float


type instruction = Plus | Minus | Mult | Div | Sin | Cos | Exp | Float of float

type stack = float list

let eval_tests = [
  (FLOAT(0.), 0.);
  (PLUS(FLOAT(2.), FLOAT(2.)), 4.);
  (MINUS(FLOAT(69.),FLOAT(9.)), 60.);
  (MULT(FLOAT(7.),FLOAT(5.)), 35.);
  (EXP(FLOAT(1.)), exp 1.); 
  (SIN(FLOAT(0.)), 0.); 
]

(* TODO: Implement eval. *)
let rec eval e = match e with
  | FLOAT (n) -> n
  | PLUS (n,m) -> eval (n) +. eval (m)
  | MINUS (n,m) -> eval (n) -. eval (m)
  | MULT (n,m) -> eval (n) *. eval (m)
  | DIV (n, m) -> eval (n) /. eval (m)
  | SIN (n) -> 
      let a = eval n in
      sin a
  | COS (n) -> 
      let a = eval n in
      cos a
  | EXP (n) -> 
      let a = eval n in
      exp a
;;
  

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (PLUS (FLOAT 2.0, FLOAT 2.0), [Float 2.0; Float 2.0; Plus]);
  (SIN (FLOAT 2.0), [Float 2.0; Sin]);
  (FLOAT(2.0), [Float 2.0]);
]

(* TODO: Implement to_instr. *)
let to_instr e = 
  let rec to_instr_aux (e : exp) (acc : instruction list) : instruction list = 
    match e with
    | FLOAT (n) -> acc @ [Float n]
    | PLUS (a,b) -> acc @ (to_instr_aux a acc) @ (to_instr_aux b acc) @ [Plus]
    | MINUS (a,b) -> acc @ (to_instr_aux a acc) @ (to_instr_aux b acc) @ [Minus] 
    | MULT (a,b) -> acc @ (to_instr_aux a acc) @ (to_instr_aux b acc) @ [Mult] 
    | DIV (a,b) -> acc @ (to_instr_aux a acc) @ (to_instr_aux b acc) @ [Div] 
    | SIN (a) -> acc @ (to_instr_aux a acc) @ [Sin]
    | COS (a) -> acc @ (to_instr_aux a acc) @ [Cos]
    | EXP (a) -> acc @ (to_instr_aux a acc) @ [Exp] 
  in
  to_instr_aux e []
;;
  


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Plus, [2.0;2.0]), Some [4.0]);
  ((Sin, [1.0]), Some [sin 1.0]);
  ((Exp, []), None);
  ((Plus, [1.0]), None);
  ((Float 69.,  [69.]), Some [ 69.; 69.]);
  ((Minus , [4.; 5.;]), Some [1.]);
]


(* TODO: Implement to_instr. *)               
let instr i s = 
  let rec size (s : stack) (acc : int) : int =
    match s with 
    | [] -> acc
    | _::t -> size t (acc+1)
  in
  
  let is_valid_size (s : stack) (i : instruction) : bool =
    match i with
    | Plus | Minus | Mult | Div ->
        (size s 0) >= 2
    | Sin | Cos | Exp -> 
        (size s 0) >= 1
    | _ -> true
  in
  
  
  let compute1 (i : instruction) (s : stack) : stack = 
    let h::t = s in
    match i with
    | Sin -> 
        (sin h)::t
    | Cos ->
        (cos h)::t 
    | Exp -> 
        (exp h)::t 
    | _ -> raise Error
  in
  
          
  let compute2 (i : instruction) (s : stack) : stack =
    let x::y::t = s in
    match i with
    | Plus -> (x +. y)::t
    | Minus -> (y -. x)::t
    | Mult -> (x *. y)::t
    | Div ->  (y /. x)::t
    | _ -> raise Error
        
  in
  
  let compute_float (i : instruction) (s : stack) : stack =
    match i with
    | Float f -> 
        begin match s with
          | [] -> f::[]
          | h::t -> f::h::t
        end
    | _ -> s
  in
  
  if (not (is_valid_size s i)) then None
  else 
    match i with
    | Plus | Minus | Mult | Div ->
        Some (compute2 i s) 
    | Sin | Cos | Exp ->
        Some (compute1 i s)
    | _ -> Some (compute_float i s)
  
;; 

(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.2 ; Float 3.3; Plus; Float 5.; Mult], Some 27.5);
  ([Float 1. ; Sin ;], Some (sin 1.));
]

(* TODO: Implement prog. *)
let prog instrs =
  
  let extract_stack (s : stack option) : stack = 
    match s with
    | None -> []
    | Some st -> st
  in
  
  let rec compute (s : stack option) (instrs : instruction list) 
    : stack option = 
    match instrs with 
    | [] -> s
    | h::t -> compute (instr h (extract_stack s)) t
  in
  
  let res = compute (Some []) (instrs) in
  
  match res with
  | None -> None
  | Some (h::[])-> Some h 
;;