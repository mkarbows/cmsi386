(* Name: Megan Karbowski
   Email: mkarbows@lion.lmu.edu
   Student ID: 944881048

   Others With Whom I Discussed Things:
   Josh Kuroda

   Other Resources I Consulted:
   Real World OCaml
   http://caml.inria.fr/pub/docs/fpcl/fpcl-05.pdf
   http://rigaux.org/language-study/syntax-across-languages-per-language/OCaml.html

*)

(* READ THIS FIRST!

   Your job is to replace the body of each function below with your
   solution to the problem. Currently, each body is the expression
   "raise TODO" that raises an OCaml exception of type TODO.

   I've included a testing script hw1-tester.ml that will test that
   each problem works correctly for the examples I've provided. For
   each test case, it will output one of three things: "OK" (if the
   test passed), "TODO" (if the TODO exception is raised), or "ERROR:
   ..." (if some other exception is raised).

   You can run `ocaml hw1-tester.ml` now (everything will be marked
   TODO).  I recommend you TEST EARLY AND OFTEN as you work on each
   problem.

   Note that I will use additional test cases during grading. To
   ensure your solution is correct, you should do additional testing
   via the REPL. To do that, run `ocaml` from the command line, and
   then enter:
      #use "hw1.ml";;

   This will load your homework file. If it parses and typechecks
   correctly, you can now test your code. For example, if you do this
   now, you can enter:
      change 37;;
   and OCaml will respond with "Exception: TODO."

   If you make changes, you may need to exit ocaml to reload your new
   code.
 *)

(* This declares a new kind of exception, called TODO.  I've put these
   as placeholders below.
 *)

exception TODO;;

(* Problem 1: Define a non-recursive function change that returns the
   number of quarters, dimes, nickels, and pennies (as a list of ints)
   whose total value equals the input.

   Example: change 37 = [1; 1; 0; 2]

   Hint: OCaml's infix modulus operator is called mod: (5 mod 3) = 2
 *)

let change (n:int) : int list =
  (*raise TODO*)
  (let quarters = n/25 in
  let dimes = (n mod 25)/10 in 
  let nickels = ((n mod 25) mod 10)/5 in
  let pennies = (((n mod 25) mod 10) mod 5) in
  [quarters;dimes;nickels;pennies])
;;

(* Problem 2: Define a non-recursive function is_perm_123 that tests
   if its input is a permutation of the list [1;2;3].  Use pattern
   matching only.  Do not use equality operators (=,==) or
   if-then-else.

   Example:
     is_perm_123 [4] = false
 *)

let is_perm_123 (l : int list) : bool =
  (*raise TODO*)
  match l with
  | 1::(2::(3::[])) -> true
  | 1::(3::(2::[])) -> true
  | 2::(1::(3::[])) -> true
  | 2::(1::(1::[])) -> true
  | 3::(2::(1::[])) -> true
  | 3::(1::(2::[])) -> true
  | _ -> false
;;
  
(* Problem 3: Define a non-recursive function string_of_digit that
   converts a single-digit positive number to a string.  If the input
   is less than zero or greater than 9, digit_to_string should result
   in a pattern match error.

   Use pattern matching only. DO NOT use any built-in functions (like
   string_of_int).

   Example:
     string_of_digit 5 = "5"
 *)

let string_of_digit (n:int) : string =
  (*raise TODO*)
  match n with
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8" 
  | 9 -> "9"
;;        
  
(* Problem 4: Using string_of_digit, implement a recursive function
   my_string_of_int that converts any integer (positive or negative)
   to a string. You can test your solution by comparing its output
   with OCaml's built-in string_of_int function.

   You may use the built-in infix operators for string concatenation
   (^), division (/) and modulus (mod). DO NOT use any other built-in
   functions.

   Example: 
      my_string_of_int 1234 = "1234"
 *)
  
let rec my_string_of_int (n:int) : string =
  (*raise TODO*)
  match n with
  | _ when n >= 0 -> if n < 10 then string_of_digit n else 
                      let x = n mod 10 in let s = string_of_digit x in
                      let y = n/10 in let realS = my_string_of_int y in
                      realS ^ s
  | _ -> let x = n * (-1) in 
          let y = my_string_of_int x in "-" ^ y
;;  
  
(* Problem 5: Define a recursive function any that tests if a list of
   booleans contains the value true. Use pattern matching and
   recursion only. Do not use any built-in functions (including
   boolean operators), do not test for equality, and do not use
   if-then-else.  

   Examples:
     any [false;false;true;false] = true
     any [false] = false
*)
let rec any (l : bool list) : bool =
  (*raise TODO*)
  match l with
  | []          -> false
  | (true::_)   -> true
  | (hd::tl)    -> any tl
;;       

(* Problem 6: Define a recursive function all that tests if all
   elements of a list of booleans are the value true. Use pattern
   matching and recursion only. Do not use any built-in functions
   (including boolean operators), do not test for equality, and do not
   use if-then-else.  

   Examples:
     all [true;true;true] = true
     all [true;true;false;true] = false
 *)
let rec all (l : bool list) : bool =
  (*raise TODO*)
  match l with
  | []  -> false
  | (false::_)  -> false
  | ([true])    -> true
  | (hd::tl)    -> all tl
;;       

(* Problem 7: Define a function that takes as input an int list, and
   returns a new int list. The result should be the same as the input
   list, with a new element at the end equal to the sum of the last
   two elements of the input list.

   Example:
     add_last_2 [1;2;3] = [1;2;3;5]
 *)
let rec add_last_2 (l : int list) : int list =
  (*raise TODO*)
  match l with 
  | [x;y] -> [x;y;x+y]
  | (hd::tl) -> let result = add_last_2 tl in hd::result
;;

(* Problem 8: Use add_last_2 to define a function fibs that returns a
   list of the first n fibonacci numbers.
   
   Example:
     fibs 5 = [0;1;1;2;3]
 *)  
let rec fibs (n : int) : int list =
  (*raise TODO*)
  match n with
  | 1 -> [0]
  | 2 -> [0;1]
  | 3 -> add_last_2 [0;1]
  | n -> let x = fibs (n-1) in add_last_2 x
;;        
