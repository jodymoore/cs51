open Core.Std
exception ImplementMe

(**************************** Part 1: Bignums *******************************)
type bignum = {neg: bool; coeffs: int list}
let base = 10
<<<<<<< HEAD
let zero = {neg = false; coeffs = []} ;;

=======
>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2

(* Please make sure you fully understand the representation invariant for
 * bignums, as documented in the problem set specification. *)

(*>* Problem 1.1 *>*)
<<<<<<< HEAD

let negate (b : bignum) : bignum  =
  match b with
  | {neg = false; coeffs = []} -> {neg = false; coeffs = []}
  | {neg = false; coeffs = _} -> {neg = true ; coeffs = b.coeffs}
  | {neg = true; coeffs = _} ->  {neg = false; coeffs = b.coeffs}    
;;
=======
let negate (b : bignum) : bignum =
  raise ImplementMe

>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2

(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)
let _ = assert(negate {neg = false; coeffs = []}
                    = {neg = false; coeffs = []})
let _ = assert(negate {neg = true; coeffs = [1; 2]}
                    = {neg = false; coeffs = [1; 2]})

(*>* Problem 1.3.1 *>*)
<<<<<<< HEAD


(* helper function for fromInt there was a 
    little bug in abs this helps fix it  *)
let change_min n = 
  let a = List.rev(n) in
match a with
| [] -> []
| h::rest ->List.rev(h + 1::rest)
;;


let fromInt (n: int) : bignum =                                      
  let int_lst d =
    let rec lst acc d =
      if d < 10 then d::acc
      else lst ((d mod 10)::acc) (d/10) in
    lst [] d in
  match n with    
  | 0 -> zero
  | n ->  if n < 0 && n = Int.min_value then {neg = true; coeffs = change_min(int_lst(abs(n - 1)))}
          else if n < 0 then {neg = true; coeffs = int_lst(abs(n))}
          else {neg = false; coeffs = int_lst(n)}
;;

let () = assert ( fromInt 23456 = {neg = false; coeffs = [2;3;4;5;6]}) ;; 
let () = assert ( fromInt(-23456) = {neg = true; coeffs = [2;3;4;5;6]}) ;; 

=======
let fromInt (n: int) : bignum =
  raise ImplementMe


>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2
(** Some helpful functions **)

(* Removes zero coefficients from the beginning of the bignum representation *)
let rec stripzeroes (b : int list) : int list =
  match b with
    | 0 :: t -> stripzeroes t
    | _ -> b


(* stripzeroes a bignum *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = stripzeroes b.coeffs}


(* Returns a random bignum from 0 to bound - 1 (inclusive).
 * Can use this to help randomly test functions. *)
let randbignum (bound : bignum) =
  let rec randbignum_rec (bound : int list) =
    match bound with
      | [] -> []
      | [h] -> if h = 0 then [] else [Random.int h]
      | _ :: t -> Random.int base :: randbignum_rec t
  in {neg = false; coeffs = List.rev (randbignum_rec (List.rev bound.coeffs))}


(** Some helpful string functions **)
(* Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s ~pos:1 ~len:(len - 1))

(* Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
    | [] -> ""
    | c :: t -> String.make 1 c ^ implode t

(** Other functions you may find useful. *)
(* Returns the first n elements of list l (or the whole list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
    | [] -> []
    | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1)

(* Returns a pair
 * (first n elements of lst, rest of elements of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h :: t -> let (lst1, lst2) = split t (n - 1) in
                (h :: lst1, lst2)

(* Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  Float.to_int (log10 (Float.of_int base))


(* fromString and toString assume the base is a power of 10 *)
(* Converts a string representing an integer to a bignum. *)
let fromString (s : string) : bignum =
  let rec fromString_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: fromString_rec rest
  in
  match explode s with
    | [] -> fromInt 0
    | h :: t -> if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false;
<<<<<<< HEAD
            coeffs = stripzeroes(List.rev (fromString_rec (List.rev (h :: t))))}
=======
            coeffs = stripzeroes (List.rev (fromString_rec (List.rev (h :: t))))}
>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2


(* Converts a bignum to its string representation.
 * Returns a string beginning with ~ for negative integers. *)
let toString (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s else
      "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s ~pos:1 ~len:(String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
      | [] -> ""
      | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                  ^ coeffs_to_string t in
  let stripped = stripzeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs


(*>* Problem 1.2 *>*)
<<<<<<< HEAD
let equal (b1 : bignum) (b2 : bignum) : bool =
  b1 = b2 ;;

let () = assert(equal {neg = false; coeffs = [2;3;4;5;6;7]} {neg = false; coeffs = [2;3;4;5;6;7]} = true) ;;
let () = assert(equal {neg = false; coeffs = [2;3;4;5;6;7]} {neg = false; coeffs = [2;3;4]} = false) ;;
=======
let rec equal (b1 : bignum) (b2 : bignum) : bool =
  raise ImplementMe

>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2

(* Automated testing function. Use this function to help you catch potential
 * edge cases. While this kind of automated testing is helpful, it is still
 * important for you to think about what cases may be difficult for your
 * algorithm. Also, think about what inputs to run the testing function on. If
 * you're having trouble isolating a bug, you can try printing out which values
 * cause an assert failure. *)
let rec test_equal (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(equal (fromInt count) (fromInt max) = (count = max)) in
    test_equal (count + 1) max


let () = test_equal (-10000) 10000
let () = test_equal 10000 (-10000)
let () = test_equal (-10000) 9999

<<<<<<< HEAD

let less (b1 : bignum) (b2 : bignum) : bool =
  let bignum1,bignum2  = List.length b1.coeffs,List.length b2.coeffs in
    if bignum2 < bignum1 then false
    else
    match b1.neg,b2.neg with
    | true,true ->  b1 > b2
    | false,false -> b1 < b2 
    | true,false -> true
    | false,true -> false  
;;


let () = assert(less {neg = false; coeffs = [2;3;4;5;6;7]} {neg = false; coeffs = [2;3;4;5;6;9;4]} = true) ;;
let () = assert(less {neg = false; coeffs = [2;3;4;5;6;7]} {neg = false; coeffs = [2;3;4]} = false) ;;

 
let greater (b1 : bignum) (b2 : bignum) : bool =
    not(less b1 b2) ;;


let () = assert(greater {neg = false; coeffs = [2;3;4;5;6;7]} {neg = false; coeffs = [2;3;4;5;6;9;4]} = false) ;;
let () = assert(greater {neg = false; coeffs = [2;3;4;5;6;7]} {neg = false; coeffs = [2;3;4]} = true) ;;


(*>* Problem 1.3.2 *>*)

(* helper function for toInt *)
 
let rec sum_list (n : int list) : int =
  match n with 
  | [] -> 0
  | x::rest ->  x + sum_list rest 
;;
(*  multiplies coeffs with base and returns an int *)
let mult_coeffs (nums:int list) : int  =                               
   let rec mult (b: int) (lst: int list) : int list   =   
     match lst with
     | [] -> []
     | [x] -> x * b :: []
     | h::rest -> h * b :: mult(b * base)(rest)  in
     sum_list(mult(1)(List.rev(stripzeroes(nums))))   
;;


let toInt (b : bignum) : int option =
  if  greater b (fromInt Int.max_value) || less b (fromInt(Int.min_value)) then None
  else 
  match b with
  | {neg = false; coeffs = []} -> Some 0
  | {neg = false; coeffs = _} -> Some(mult_coeffs b.coeffs)
  | {neg = true;  coeffs = _}  -> Some((mult_coeffs b.coeffs) * -1) 
;;
=======
let less (b1 : bignum) (b2 : bignum) : bool =
  raise ImplementMe


let greater (b1 : bignum) (b2 : bignum) : bool =
  raise ImplementMe


(*>* Problem 1.3.2 *>*)
let toInt (b : bignum) : int option =
  raise ImplementMe
>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2


(** Some arithmetic functions **)

(* Returns a bignum representing b1 + b2.
 * Assumes that b1 + b2 > 0. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
            : bool * int list =
    match (coeffs1, coeffs2) with
      | ([], []) -> pair_from_carry carry
      | ([], _) -> if carry = 0 then (neg2, coeffs2) else
          plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
      | (_, []) -> if carry = 0 then (neg1, coeffs1) else
          plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
      | (h1 :: t1, h2 :: t2) ->
          let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
          let result = h1 * sign1 + h2 * sign2 + carry in
          if result < 0 then
            let (negres, coeffsres) =
                  plus_with_carry (neg1, t1) (neg2, t2) (-1)
            in (negres, result + base :: coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, result - base :: coeffsres)
          else
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
        plus_with_carry (b1.neg, List.rev b1.coeffs)
                        (b2.neg, List.rev b2.coeffs)
                        0
  in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)}


(*>* Problem 1.4 *>*)
(* Returns a bignum representing b1 + b2.
 * Does not make the above assumption.
 * Hint: How can you use plus_pos to implement this?
*)
<<<<<<< HEAD

let plus (b1 : bignum) (b2 : bignum) : bignum =
   match b1.neg,b2.neg with
   | false,false  -> plus_pos b1 b2  
   | true,false -> let new_num = negate b1 in 
                   negate(plus_pos new_num b2)
   | false,true -> let new_num2 = negate b2 in 
                   negate(plus_pos new_num2 b1)
   | true,true -> let n1,n2 = negate b1,negate b2 in
                   negate(plus_pos n1 n2)
;;
=======
let plus (b1 : bignum) (b2 : bignum) : bignum =
  raise ImplementMe
>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2


(*>* Problem 1.5 *>*)
(* Returns a bignum representing b1*b2.
 * Things to think about:
 * 1. How did you learn to multiply in grade school?
 * (In case you were never taught to multiply and instead started with
 * set theory in third grade, here's an example):
 *
 *      543
 *     x 42
 *     ____
 *     1086
 *   +21720
 *   =22806
 *
 * 2. Can you use any functions you've already written?
 * 3. How can you break this problem into a few simpler, easier-to-implement
 * problems?
 * 4. If your code is buggy, test helper functions individually instead of
 * the entire set at once.
 * 5. Assuming positivity in some of your helper functions is okay to
 * simplify code, as long as you respect that invariant.
*)
<<<<<<< HEAD

(* helper function for mult2 places an int in a list *)  
let int_lst d =
    let rec lst acc d =
      if d < 10 then d::acc
      else lst ((d mod 10)::acc) (d/10) in
    lst [] d 
;;

(* this is a helper function for list_producer multiplies every elem in list with n *)

 let rec mult2 (n: int) (lst: int list)(carry : int) : int list   =  
         match lst with
         | [] -> []  
         | [x] ->  let (left_side,right_side) =  split(int_lst(x*n + carry)) 1 in
                     if x * n >= base then mult_coeffs right_side::mult_coeffs left_side::[] 
                     else if  x * n + carry >= base then mult_coeffs right_side::mult_coeffs left_side::[] 
                     else x * n + carry :: []                
         | h::rest ->                         
                     (match carry with 
                     | 0 -> let (left_side,right_side) =  split(int_lst(h*n)) 1 in 
                              if h * n >= base then mult_coeffs right_side::mult2(n)(rest)(mult_coeffs left_side)                            
                              else h * n::mult2(n)(rest)(0)
                     | _ -> let (left_side,right_side) =  split(int_lst((h*n) + carry)) 1 in  
                              if h * n  >= base then mult_coeffs right_side:: mult2(n)(rest)(mult_coeffs left_side) 
                              else if carry > 0 && h = 0  then n + carry::mult2(n)(rest)(0)
                              else h * n::mult2(n)(rest)(0))                                
;; 


(* recusively adds 0s to a list *)
let rec shift_by_base (b: int list)(n:int) =
  if b = [] then b 
  else if n > 0 then shift_by_base (b @ [0])(n-1)
  else b              
;;

(* reverses lists in a list of lists *)
(* goes through a list of list reversing each list *)
let reverse (rows:int list list) : int list list =
    List.map  ~f:(fun x -> List.rev(x)) rows
;;
 
let rec coeffs_to_string (coeffs : int list) : string =
    let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s else
      "0" ^ pad_with_zeroes_left s (len - 1) in
    match coeffs with
      | [] -> ""
      | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                  ^ coeffs_to_string t 
;;
(* feeds a bignum list to plus_pos *)
let sum_bignum (b : bignum list): bignum =
   let fold_function (acc : bignum) (elem : bignum) : bignum =
   plus_pos acc elem 
   in
   List.fold_left ~f:(fun x y -> fold_function x y) ~init:zero b
;;


let times (b1 : bignum) (b2 : bignum) : bignum =
  let lst_producer coeffs1 coeffs2 =
    (* reverse the lists to multply *)
    let lst,multiplier_list = List.rev coeffs1,List.rev coeffs2 in
      (* helper function calls on mult2 to multiply lst with multiplier list *)
      let rec helper m = 
        match m with 
        | [] -> []
        | h::t -> (mult2 h lst 0) :: (helper t) in 
        (* reverse list after multiplied *)
        let result = reverse(helper multiplier_list)   in
          (* shift base with zeroes *)
          let add_zeroes = List.mapi ~f:(fun i x -> shift_by_base x i)result in
            (* to string to call on fromInt *)
            let to_str =  List.map ~f:(fun i -> coeffs_to_string i) add_zeroes  in
              (*  place in list of bignums *)
              let lst_of_bigs = List.map ~f:(fun x -> fromString(x)) to_str in
              (* sum bignums *)
              sum_bignum lst_of_bigs  in
    lst_producer b1.coeffs b2.coeffs   
;;

let () = assert (times {neg = false; coeffs = [1; 7; 0; 4; 0]} {neg = false; coeffs = [1; 1; 3; 6]} =
                       {neg = false; coeffs = [1; 9; 3; 6; 1; 0; 4; 0]}) ;;                    
=======
let times (b1 : bignum) (b2 : bignum) : bignum =
  raise ImplementMe

>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2

(* Returns a bignum representing b/n, where n is an integer less than base *)
let divsing (b : int list) (n : int) : int list * int =
  let rec divsing_rec (b : int list) (r : int) : int list * int =
    match b with
      | [] -> [], r
      | h :: t ->
          let dividend = r * base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot * n) in
          (quot :: q, r) in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1 :: h2 :: t -> if h1 < n then divsing_rec (h1 * base + h2 ::t) 0
        else divsing_rec b 0


(* Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let divmod (b1 : bignum) (b2 : bignum): bignum * bignum =
  let rec divmod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m) else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
        | [] -> failwith "Division by zero"
        | ns :: _ -> let (p, _) =
            if ns + 1 = base then
              (take_first mc (List.length mc - List.length nc), 0)
            else
              let den = ns + 1 in
              let num = take_first mc (List.length mc - List.length nc + 1)
              in divsing num den
          in
          let bp = clean {neg = false; coeffs = p} in
          let p2 = clean (if equal bp (fromInt 0) then fromInt 1 else bp) in
            divmod_rec (clean (plus m (negate (times n p2))))
                       (clean n)
                       (clean (plus psum p2))
  in
  divmod_rec (clean b1) (clean b2) (fromInt 0)
<<<<<<< HEAD
(*
=======

>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2

(**************************** Challenge 1: RSA ******************************)

(** Support code for RSA **)
(* Hint: each part of this problem can be implemented in approximately one
 * line of code. *)

(* Returns b to the power of e mod m *)
let rec expmod (b : bignum) (e : bignum) (m : bignum) : bignum =
  if equal e (fromInt 0) then fromInt 1
  else if equal e (fromInt 1) then
    snd (divmod (clean b) (clean m))
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = expmod (clean b) q (clean m) in
    let (_, x) = divmod (times (times res res) (expmod (clean b) r (clean m)))
                        (clean m) in
    {neg = x.neg; coeffs = stripzeroes x.coeffs}

(* Returns b to the power of e *)
let rec exponent (b : bignum) (e : bignum) : bignum =
  if equal (clean e) (fromInt 0) then fromInt 1
  else if equal (clean e) (fromInt 1) then clean b
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = exponent (clean b) q in
    let exp = (times (times res res) (exponent (clean b) r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs}

(* Returns true if n is prime, false otherwise. *)
let isPrime (n : bignum) : bool =
  let rec miller_rabin (k : int) (d : bignum) (s : int) : bool =
    if k < 0 then true else
    let rec square (r : int) (x : bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) n in

        if equal x (fromInt 1) then false
        else if equal x (plus n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if equal x (fromInt 1) || equal x (plus n (fromInt (-1))) then
        miller_rabin (k - 1) d s
      else square 1 x
  in
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n : bignum) (s : int) =
    let (q, r) = divmod n (fromInt 2) in
      if equal r (fromInt 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = divmod n (fromInt 2) in
    if equal r (fromInt 0) then false else
      let (d, s) = factor (plus n (fromInt (-1))) 0 in
        miller_rabin 20 d s

(* Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m : bignum) (d : bignum) : bignum * bignum * bignum =
  if equal d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod m d in
    let (s, t, g) = euclid d r in
      (clean t, clean (plus s (negate (times q t))), clean g)


(* Generate a random prime number between min and max-1 (inclusive) *)
let rec generateRandomPrime (min : bignum) (max: bignum) : bignum =
  let rand = plus (randbignum (plus max (negate min))) min in
    if isPrime rand then rand else generateRandomPrime min max


(** Code for encrypting and decrypting messages using RSA **)

(* Generate a random RSA key pair, returned as (e, d, n).
 * p and q will be between 2^n and 2^(n+1).
 * Recall that (n, e) is the public key, and (n, d) is the private key. *)
let rec generateKeyPair (r : bignum) : bignum * bignum * bignum =
  let c1 = fromInt 1 in
  let c2 = fromInt 2 in
  let p = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  let rec selectPair () =
    let e = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (clean e, clean d, clean (times p q))
      else selectPair ()
  in
    if equal p q then generateKeyPair r else selectPair ()


(*>* Problem 2.1 *>*)

(* To encrypt, pass in n e s. To decrypt, pass in n d s. *)
let encryptDecryptBignum (n : bignum) (e : bignum) (s : bignum) : bignum =
  raise ImplementMe


(* Pack a list of chars as a list of bignums, with m chars to a bignum. *)
let rec charsToBignums (lst : char list) (m : int) : bignum list =
  let rec encchars lst =
    match lst with
      | [] -> (fromInt 0)
      | c :: t -> clean (plus (times (encchars t) (fromInt 256))
                              (fromInt (Char.to_int c)))
  in
    match lst with
      | [] -> []
      | _ -> let (enclist, rest) = split lst m in
             encchars enclist :: charsToBignums rest m


(* Unpack a list of bignums into chars (reverse of charsToBignums) *)
let rec bignumsToChars (lst : bignum list) : char list =
  let rec decbignum b =
    if equal b (fromInt 0) then []
    else let (q, r) = divmod b (fromInt 256) in
      match toInt r with
        | None -> failwith "bignumsToChars: representation invariant broken"
        | Some ir -> Char.of_int_exn ir :: decbignum q
  in
    match lst with
      | [] -> []
      | b :: t -> decbignum b @ bignumsToChars t


(* Return the number of bytes required to represent an RSA modulus. *)
let bytesInKey (n : bignum) =
  Float.to_int (Float.of_int (List.length (stripzeroes n.coeffs) - 1)
                *. log10 (Float.of_int base) /. (log10 2. *. 8.))


(* Encrypts or decrypts a list of bignums using RSA.
 * To encrypt, pass in n e lst.
 * To decrypt, pass in n d lst. *)
let rec encDecBignumList (n : bignum) (e : bignum) (lst : bignum list) =
  match lst with
    | [] -> []
    | h :: t -> encryptDecryptBignum n e h :: encDecBignumList n e t



(*>* Problem 2.2 *>*)
let encrypt (n : bignum) (e : bignum) (s : string) =
  raise ImplementMe


(* Decrypt an encrypted message (list of bignums) to produce the
 * original string. *)
let decrypt (n : bignum) (d : bignum) (m : bignum list) =
  raise ImplementMe


(**************** Challenge 2: Faster Multiplication *********************)

(* Returns a bignum representing b1*b2 *)
let times_faster (b1 : bignum) (b2 : bignum) : bignum =
  raise ImplementMe
<<<<<<< HEAD
*)

let minutes_spent = 1000
=======


let minutes_spent = raise ImplementMe
>>>>>>> d85e79d39c2a24bc5c7a420fc5718d31cf556cd2
