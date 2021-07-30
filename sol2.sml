(* Simple Eval *)

datatype expr = NUM of int
|	PLUS of expr * expr
|	MINUS of expr * expr;

datatype formula = TRUE
|	FALSE
|	NOT of formula
|	ANDALSO of formula * formula
|	ORELSE of formula * formula
|	IMPLY of formula * formula
|	LESS of expr * expr;

fun eval e =
	case e of
		TRUE => true
	|	FALSE => false
	|	NOT e1 => if eval e1 then false else true
	|	ANDALSO(e1, e2) => if eval e1 then eval e2 else false
	|	ORELSE(e1, e2) => if eval e1 then true else eval e2
	|	IMPLY(e1, e2) => if eval e1 then eval e2 else true
	|	LESS(a, b) => 
		let
			fun expr_helper x =
				case x of
					NUM x => x
				|	PLUS(x, y) => expr_helper x + expr_helper y
				|	MINUS(x, y) => expr_helper x - expr_helper y
		in
		(expr_helper a < expr_helper b)
		end;

(* Check MetroMap *)

type name = string

datatype metro = STATION of name
|	AREA of name * metro
|	CONNECT of metro * metro;

fun checkMetro m =	
	case m of
		STATION m1 => false
	|	CONNECT(m1, m2) => false
	|	AREA(n1, m1) => (
			let fun checkMetro_helper x =
						case x of
							STATION met1 => STATION met1
						|	AREA(nam1, met1) => (
								case met1 of 
									CONNECT(con1, con2) =>
										let val cMh_con1 = checkMetro_helper(con1);
											val cMh_con2 = checkMetro_helper(con2);
										in
											if STATION nam1 = cMh_con1 then cMh_con2 
											else if STATION nam1 = cMh_con2 then cMh_con1 
											else met1 
										end
								| _ =>
									let val temp1 = checkMetro_helper(met1) in
										if STATION nam1 = temp1 then STATION "" else temp1
									end)
			in			
			case m1 of
				CONNECT(a1, a2) => if checkMetro_helper a1 = checkMetro_helper a2 andalso STATION n1 = checkMetro_helper a1 then true else false
			| _ => if STATION n1 = checkMetro_helper(m1) orelse checkMetro_helper(m1) = STATION "" then true else false
			end);
	
(* Lazy Lists - i *)

datatype 'a lazyList = nullList
|	cons of 'a * (unit -> 'a lazyList);

fun seq(first:int, last:int) =
	if first > last then nullList else cons(first, fn() => seq(first + 1, last));
	
fun infSeq(first:int) = cons(first, fn() => infSeq(first + 1));

fun firstN(_, 0) = []
|	firstN(nullList, _) = []
|	firstN(cons(x, t), n) = x::firstN(t(), n - 1);

fun Nth(nullList, _) = NONE
|	Nth(cons(x, t), 1) = SOME x
|	Nth(cons(x, t), n:int) = Nth(t(), n - 1); 

fun filterMultiples(nullList, _) = nullList
|	filterMultiples(cons(x, t), n:int) = if (x mod n = 0) then cons(x, fn() => filterMultiples(t(), n))
	else filterMultiples(t(), n);

(* Lazy Lists - ii *)

fun primes() = 
	let	val infList = infSeq(2);
	
		fun sieve_helper(cons(x1, t1), n:int) = if (x1 mod n <> 0) then cons(x1, fn() => sieve_helper(t1(), n))
			else sieve_helper(t1(), n);
		fun sieve(cons(x, t)) = cons(x, fn() => sieve(sieve_helper(t(), x)))
	in
		sieve(infList)
	end;