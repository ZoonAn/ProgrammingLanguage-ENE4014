datatype pattern = Wildcard | Variable of string | UnitP
|	ConstP of int | TupleP of pattern list
|	ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list
|	Constructor of string * valu

(* check_pat *)

fun check_pat p =
	let
		fun check_pat_helper pat =
			case pat of
				Variable x => [x]
			|	TupleP tp => List.foldl(fn(y, ys) => ys @ check_pat_helper y) [] tp
			|	_ => []
		
		fun repeat_check([]) = false
		|	repeat_check(s::ss) = List.exists (fn a => s = a) ss orelse repeat_check(ss)		
	in
		not (repeat_check(check_pat_helper p))
	end
	
(* match *)

fun match(v, p) = 
		case (v, p) of
			(_, Wildcard) => SOME []
		|	(v, Variable pv) => SOME [(v,Variable pv)]
		|	(Unit, UnitP) => SOME []
		|	(Const vc, ConstP pc) => if vc = pc then SOME [] else NONE
		|	(Tuple vs, TupleP ps) => if length vs = length ps then
					let 
						val z = ListPair.zip(vs, ps);
						val ls = List.filter (fn(x,y) => if isSome (match(x,y)) then true else false) z
					in
						if ls = z then NONE else SOME ls
					end
				else NONE
		|	(Constructor (s2, vc), ConstructorP(s1, pc)) => if s1 = s2 then match(vc, pc) else NONE
		|	_ => NONE
		
(* 3 *)

type name = string

datatype RSP =
	ROCK
|	SCISSORS
|	PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament =
	PLAYER of name * (RSP strategy ref)
|	MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))

fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))

fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
	let val Cons(rsp, func) = !strategyRef in
		strategyRef := func();
		rsp
	end

fun whosWinner(t:tournament) = 
	case t of
		PLAYER t => PLAYER t
	|	MATCH (t1, t2) =>
			let
				val t1_m = whosWinner t1;
				val t2_m = whosWinner t2;
				fun whosWinner_helper(strt1, strt2) =
					let
						val rsp1 = next(strt1);
						val rsp2 = next(strt2);
					in
						if rsp1 = rsp2 then whosWinner_helper(strt1, strt2)
						else if (rsp1 = ROCK andalso rsp2 = SCISSORS)
						orelse (rsp1 = SCISSORS andalso rsp2 = PAPER)
						orelse (rsp1 = PAPER andalso rsp2 = ROCK) then true
						else false
					end
			in
				case (t1_m, t2_m) of
					(PLAYER (p1_name, p1_strt), PLAYER (p2_name, p2_strt)) =>
						if whosWinner_helper(p1_strt, p2_strt) then t1_m
						else t2_m
						
			end
