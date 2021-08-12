:- use_module(library(tokenize)).
%using Table instead of eliminating left recursion because tabling helps us in preserving associativity.
:- table declaration/3,commands/3,expr/3, term/3, boolean/3.

:-consult('semantics.pl').

simple(File):-tokenize_file(File, L,[cntrl(false),spaces(false)]),strip_category(L,Res),program(Tree,Res,[]),eval_program(Tree,[],_Env).

strip_category([],[]).
strip_category([H|T],[X|R]):-H = word(X),strip_category(T,R).
strip_category([H|T],[X|R]):-H = number(X),strip_category(T,R).
strip_category([H|T],['"',X,'"'|R]):-H = string(X),strip_category(T,R).
strip_category([H|T],[X|R]):-H = punct(X),strip_category(T,R).
strip_category([H|T],R):-H = space(_X),strip_category(T,R).
strip_category([H|T],R):-H = cntrl(_X),strip_category(T,R).

% DCG for Parse tree generation starts here.
program(T) --> ['begin'],block(T),['end'].
program(T) --> ['/'],['*'],singlecomment(_X),['*'],['/'],['begin'],block(T),['end'].

% block tree which branches out to declarations and commands.
block(T) --> declaration(D),[';'],commands(C),{T=block(D,C)}.
block(T) --> commands(C),{T=block(C)}.

singlecomment(T) --> [T1],{atom(T1)},singlecomment(T).
singlecomment(T) --> [T],{atom(T)}.

% declaration tree
declaration(T) --> declaration(D1), [';'], declaration(D2),{T=declarations(D1,D2)}.

% declaration and intialization for data-types int, bool and string
declaration(T) --> ['int'],identifier(I),['='],number(N),{T=intInit(I,N)}.
declaration(T) --> ['int'],identifier(I),{T=intDeclare(I)}.
declaration(T) --> ['bool'],identifier(I),{T=boolDeclare(I)}.
declaration(T) --> ['bool'],identifier(I),['='],boolean(B),{T=boolInit(I,B)}.
declaration(T) --> ['string'],identifier(I),{T=stringDeclare(I)}.
declaration(T) --> ['string'],identifier(I),['='],string(S),{T=stringInit(I,S)}.

% string 
string(T) --> ['"'],[X],['"'],{atom(X),atom_string(X,T)}.


%commands tree
commands(T) --> commands(C1),[';'], commands(C2) , {T = commands(C1,C2)}.

% Assigning value of an expression to an identifier
commands(T) --> identifier(I), ['='], expr(E),{T = assignToIdentifier(I,E)}.

% if-then-else structure
commands(T) --> ['if'],['('], boolean(B),[')'], ['then'],['{'], commands(C1),['}'], ['else'],['{'], commands(C2),['}'] ,{T = ifThenElse(B,C1,C2)}.

% while structute
commands(T) --> ['while'],['('], boolean(B),[')'],['{'], commands(C),['}'] , {T = while(B,C)}.

% Assiging boolean expression to an identifier
commands(T) --> identifier(I), ['='], boolean(B), ['?'],  expr(E1), [':'], expr(E2),{T=ternary(I,B,E1,E2)}.

% print structure
commands(T) --> ['print'],object(O),{T=print(O)}.

% for-loop structure
commands(T) --> ['for'], ['('], ['int'], identifier(I) , ['='], expr(E), [';'],boolean(B),[';'], unary(U),[')'],['{'],commands(C),['}'],{T=forloop(I,E,B,U,C)}.

% for-in-range strcture
commands(T) --> ['for'],identifier(I),['in'],['range'],['('],expr(E1),[','],expr(E2),[')'],['{'],commands(C),['}'],{T=forInRange(I,E1,E2,C)}.

% Tree for declaration in commands
commands(T) --> declaration(D),{T=declaration(D)}.


% helper for print
object(T) --> identifier(I),{T=identifier(I)}.
object(T) --> number(N),{T=number(N)}.
object(T) --> string(S),{T=string(S)}.
% if 2 strings are given directly i.e without variables, we concatinate them here.
object(T) --> string(S1),['+'],string(S2),{string_concat(S1,S2,S3),T=string(S3)}.
% if 2 strings are passed as variables to concat and print.
object(T) --> identifier(I1),['+'],identifier(I2),{T= +(I1,I2)}.

% unary operators structures
unary(T) --> identifier(I), ['+'],['+'],{T= ++(I)}.
unary(T) --> identifier(I), ['-'],['-'],{T= --(I)}.

% boolean terminals and boolean expression tree.
boolean(T) --> ['true'],{T = (true)} | ['false'],{T = (false)} |expr(E1), ['='],['='], expr(E2),{T = isEqual(E1,E2)}| ['not'], boolean(B),{T = not(B)}.
boolean(T) --> expr(E1), ['!'],['='], expr(E2),{T = isNotEqual(E1,E2)}.
boolean(T) --> expr(E1), ['<'], expr(E2),{T = isLessThan(E1,E2)}.
boolean(T) --> expr(E1), ['<'],['='], expr(E2),{T = isLessThanOrEqual(E1,E2)}.
boolean(T) --> expr(E1), ['>'], expr(E2),{T = isGreaterThan(E1,E2)}.
boolean(T) --> expr(E1), ['>'],['='], expr(E2),{T = isGreaterThanOrEqual(E1,E2)}.
boolean(T) --> booleanOperations(T).

% boolean operations structure
booleanOperations(T) --> boolean(B1),['and'],boolean(B2),{T=booleanAnd(B1,B2)}.
booleanOperations(T) --> boolean(B1),['or'],boolean(B2),{T=booleanOr(B1,B2)}.
booleanOperations(T) --> ['('],booleanOperations(T),[')'].


% expressions with + and – symbols.
expr(T) --> expr(E), ['+'], term(M),{T = +(E,M)} | expr(E), ['-'], term(M),{T = -(E,M)} | term(T).

% term represents expressions with * and /
term(T) --> term(M), ['*'], var(V), {T = *(M,V)} | term(M), ['/'], var(V),{T = /(M,V)} | var(T).

% var is like a helper predicate which takes care of parenthesis, numbers and identifiers.
var(T) --> ['('],expr(T),[')'] |identifier(T) | number(T) | string(T).

% identifier predicate to write the identifier rule.
identifier(I) --> [I],{atom(I)}.

% number predicate to write the number rule.
number(N) --> [N],{number(N)}.

