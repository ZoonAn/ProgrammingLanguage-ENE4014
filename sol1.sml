(* merge *)

fun merge ([], ys : int list) = ys
|	merge (xs : int list, []) = xs
|	merge(a::xs , b::ys) = 
		if a < b
		then a::merge(xs , b::ys)
		else b::merge(a::xs, ys);

(* reverse *)
		
fun reverse (xs : int list) =
	if xs = [] then []
	else reverse(tl(xs)) @ [hd(xs)];
	
(* pi *)
	
fun pi (a : int, b : int, f : int -> int) =
	if a > b then 1
	else if a = b then f(a)
	else f(a) + pi(a + 1, b, f);

(* digits *)

fun digits (x : int) =
	if x div 10 = 0 then [x]
	else merge ([x - (10 * (x div 10))], digits (x div 10));

(* sum - helper func. *)

fun sum ([]) = 0
|	sum (x::xs) = x + sum (xs);
	

(* additivePersistence *)

fun additivePersistence (x : int) = 
	if x div 10 = 0 then 0
	else additivePersistence ( sum ( digits(x) ) ) + 1;
	
(* digitalRoot *)

fun digitalRoot (x : int) =
	if x div 10 = 0 then x
	else digitalRoot ( sum ( digits(x) ) );