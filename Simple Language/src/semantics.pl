% lookup is to get the value of a variable.
lookup(Id,[(Id,Val)|_],Val).
lookup(Id,[H|T],Val):-(Id,Val) \= H,lookup(Id,T,Val).
lookup(Id,[],_):-string_concat(Id," Variable not found.\n",S),throw(S).

% silentlookup is to stop user from re declaring variables.
silentlookup(Id,Env):-member((Id,_),Env),string_concat("\n Can't redeclare variable ",Id,S),throw(S).
silentlookup(_Id,_Env).

% update is to set the value of a variable.
update(Id,Val,[(Id,_)|T],[(Id,Val)|T]).
update(Id,Val,[H|T],[H|Env]):-H \= (Id,_),update(Id,Val,T,Env).
update(Id,Val,[],[(Id,Val)]).

% checks if declared datatype of identifier and value are same.
typecheck(I,V,Env,true):-lookup(I,Env,V1),number(V1),number(V).
typecheck(I,V,Env,true):-lookup(I,Env,V1),string(V1),string(V).
typecheck(I,V,Env,true):-lookup(I,Env,V1),atom(V1),atom(V).

% evalutaing with semantics starts here

% evaluation for the entire parse tree starts here
eval_program(Tree,Env,NewEnv):-eval_block(Tree,Env,NewEnv).

% evaluation for the block which contains declarations and commands.
eval_block(block(D,C),Env,NewEnv):-eval_declarations(D,Env,E1),eval_commands(C,E1,NewEnv).
eval_block(block(C),Env,NewEnv):-eval_commands(C,Env,NewEnv).

% evaluate declarations
eval_declarations(declarations(D1,D2),Env,NewEnv):-eval_declarations(D1,Env,NE1),eval_declarations(D2,NE1,NewEnv).

% evaluation for data-types
eval_declarations(intInit(I,N),Env,NewEnv):-silentlookup(I,Env),update(I,N,Env,NewEnv).
eval_declarations(intDeclare(I),Env,NewEnv):-silentlookup(I,Env),update(I,0,Env,NewEnv).
eval_declarations(boolInit(I,B),Env,NewEnv):-silentlookup(I,Env),update(I,B,Env,NewEnv).
eval_declarations(boolDeclare(I),Env,NewEnv):-silentlookup(I,Env),update(I,false,Env,NewEnv).
eval_declarations(stringInit(I,S),Env,NewEnv):-silentlookup(I,Env),update(I,S,Env,NewEnv).
eval_declarations(stringDeclare(I),Env,NewEnv):-silentlookup(I,Env),update(I,null,Env,NewEnv).

% evaluate commands
eval_commands(commands(C1,C2),Env,NewEnv):-eval_commands(C1,Env,NE1),eval_commands(C2,NE1,NewEnv).

eval_commands(assignToIdentifier(I,E),Env,NewEnv):-eval_expr(E,Env,NE1,R),typecheck(I,R,NE1,true),update(I,R,NE1,NewEnv).
eval_commands(assignToIdentifier(I,E),Env,_NewEnv):-eval_expr(E,Env,NE1,R),not(typecheck(I,R,NE1,true)),throw("Error: tried to assign value of a different datatype to an identifier.\n").

% evaluation for if-then-else
eval_commands(ifThenElse(B,C1,_),Env,NewEnv):-eval_bool(B,Env,NE1,true),eval_commands(C1,NE1,NewEnv).
eval_commands(ifThenElse(B,_,C2),Env,NewEnv):-eval_bool(B,Env,NE1,false),eval_commands(C2,NE1,NewEnv).

% evaluation for while loop
eval_commands(while(B,C),Env,NewEnv):-eval_bool(B,Env,NE1,true),eval_commands(C,NE1,NE2),eval_commands(while(B,C),NE2,NewEnv).
eval_commands(while(B,_),Env,NewEnv):-eval_bool(B,Env,NewEnv,false).

% evaluation for print statement
eval_commands(print(P),Env,Env):- eval_print(P,Env).

% evaluation for ternary
eval_commands(ternary(A,B,E1,_E2),Env,NewEnv):-eval_bool(B,Env,NE1,true),eval_expr(E1,NE1,NE2,Val),update(A,Val,NE2,NewEnv).
eval_commands(ternary(A,B,_E1,E2),Env,NewEnv):-eval_bool(B,Env,NE1,false),eval_expr(E2,NE1,NE2,Val),update(A,Val,NE2,NewEnv).

% evaluation for in range loop
eval_commands(forInRange(A,B,C,D),Env,NewEnv):-eval_expr(B,Env,E1,R1),eval_expr(C,E1,E2,R2),run_loop(A,R1,R2,D,E2,NewEnv).

% evaluation of traditional for-loop
eval_commands(forloop(I,E,B,U,C),Env,NewEnv):- eval_expr(E,Env,NE1,R1),number(R1),update(I,R1,NE1,E1),for_loop(I,E,B,U,C,E1,NewEnv).
eval_commands(forloop(_I,E,_B,_U,_C),Env,_NewEnv):- eval_expr(E,Env,_NE1,R1),not(number(R1)),throw("Error: tried to assign value of a different dataype to an identifier.\n").

% evaluation for declarations
eval_commands(declaration(D),Env,NewEnv):-eval_declarations(D,Env,NewEnv).

% evaluation for identifier
eval_print(identifier(I),Env):-lookup(I,Env,Val), write(Val).

% evaluation for numbers
eval_print(number(N),_Env):-write(N).

% evaluation for string.
eval_print(string(S),_Env):-write(S).

% evaluation for string concat with variables.
eval_print( +(I1,I2),Env):- lookup(I1,Env,V1),string(V1),lookup(I2,Env,V2),string(V2),string_concat(V1,V2,V3),write(V3).

% for loop helper 1
for_loop(_I,_E,B,_U,_C,Env,NewEnv):-eval_bool(B,Env,NewEnv,false).
for_loop(I,E,B,U,C,Env,NewEnv):-eval_bool(B,Env,E2,true),eval_commands(C,E2,E3),eval_unary(U,E3,E4),for_loop(I,E,B,U,C,E4,NewEnv).

% for loop helper 2
run_loop(A,R1,R1,_C,Env,NewEnv):-update(A,R1,Env,NewEnv). 
run_loop(A,R1,R2,C,Env,NewEnv):- R1<R2,update(A,R1,Env,E1),eval_commands(C,E1,E2),R3 is R1+1,run_loop(A,R3,R2,C,E2,NewEnv).

% makesure same type values are checked for boolean arithmetic.
comparetypecheck(V1,V2,true):- number(V1),number(V2).

% evaluation for boolean true and false
eval_bool((true),Env,Env,true).
eval_bool((false),Env,Env,false).

% evaluation for boolean equality
eval_bool(isEqual(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),R1 = R2.
eval_bool(isEqual(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),R1 \= R2.

% evaluation for boolean not equality
eval_bool(isNotEqual(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),R1 = R2.
eval_bool(isNotEqual(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),R1 \= R2.

% evaluation for boolean greater than
eval_bool(isGreaterThan(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 > R2.
eval_bool(isGreaterThan(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 =< R2.
eval_bool(isGreaterThan(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").
eval_bool(isGreaterThan(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").

% evaluation for boolean lesser than
eval_bool(isLessThan(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 < R2.
eval_bool(isLessThan(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 >= R2.
eval_bool(isLessThan(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").
eval_bool(isLessThan(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").

% evaluation for boolean greater than equal to
eval_bool(isGreaterThanOrEqual(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 >= R2.
eval_bool(isGreaterThanOrEqual(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 < R2.
eval_bool(isGreaterThanOrEqual(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").
eval_bool(isGreaterThanOrEqual(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").

% evaluation for boolean lesser than equal to
eval_bool(isLessThanOrEqual(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 =< R2.
eval_bool(isLessThanOrEqual(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),comparetypecheck(R1,R2,true),R1 > R2.
eval_bool(isLessThanOrEqual(E1,E2),Env,NewEnv,true):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").
eval_bool(isLessThanOrEqual(E1,E2),Env,NewEnv,false):-eval_expr(E1,Env,NE1,R1),eval_expr(E2,NE1,NewEnv,R2),not(comparetypecheck(R1,R2,true)),throw("Can't compare different datatypes.\n").

% evaluation for boolean not
eval_bool(not(B),Env,NewEnv,false):-eval_bool(B,Env,NewEnv,true).
eval_bool(not(B),Env,NewEnv,true):-eval_bool(B,Env,NewEnv,false).

% evaluation for boolean and
eval_bool(booleanAnd(X,Y),Env,NewEnv,Val):- eval_bool(X,Env,E1,V1),eval_bool(Y,E1,NewEnv,V2),and(V1,V2,Val).

% evaluation for boolean or
eval_bool(booleanOr(X,Y),Env,NewEnv,Val):- eval_bool(X,Env,E1,V1),eval_bool(Y,E1,NewEnv,V2),or(V1,V2,Val).

% helper for evaluation of boolean and & or
is_true(X):-X = 'true'.
is_false(X):-X = 'false'.

% helper for boolean and
and(X,Y,true):-is_true(X),is_true(Y).
and(X,Y,false):-is_false(X);is_false(Y).

% helper for boolean or
or(X,Y,true):-is_true(X);is_true(Y).
or(X,Y,false):-is_false(X),is_false(Y).

% evaluation for unary operator increment 
eval_unary(++(I), Env,ER):- lookup(I, Env,Val), NewVal is Val +1, update(I, NewVal, Env,ER).

% evaluation for unary operator decrement 
eval_unary(--(I), Env,ER):- lookup(I, Env,Val), NewVal is Val -1, update(I, NewVal, Env,ER).

% evaluation for expressions
eval_expr(+(X,Y),Env,NewEnv,Val):-eval_expr(X,Env,E1,R1),eval_expr(Y,E1,NewEnv,R2),number(R1),number(R2),Val is R1 + R2.
eval_expr(-(X,Y),Env,NewEnv,Val):-eval_expr(X,Env,E1,R1),eval_expr(Y,E1,NewEnv,R2),number(R1),number(R2),Val is R1 - R2.
eval_expr(*(X,Y),Env,NewEnv,Val):-eval_expr(X,Env,E1,R1),eval_expr(Y,E1,NewEnv,R2),number(R1),number(R2),Val is R1 * R2.
eval_expr(/(X,Y),Env,NewEnv,Val):-eval_expr(X,Env,E1,R1),eval_expr(Y,E1,NewEnv,R2),number(R1),number(R2),Val is R1 / R2.
eval_expr(+(X,Y),Env,NewEnv,Val):-eval_expr(X,Env,E1,R1),eval_expr(Y,E1,NewEnv,R2),string(R1),string(R2),string_concat(R1,R2,Val).
eval_expr(N,Env,Env,N):-number(N).
eval_expr(S,Env,Env,S):-string(S).
eval_expr(Id,Env,Env,Val):-atom(Id),lookup(Id,Env,Val).