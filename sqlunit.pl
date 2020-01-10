:- module(sqlunit, [table_sqlunit_sqlquery/3]).

:- use_module(library(option)).
:- use_module(library(optparse)).

:- use_module(library(dcg_util)).

:- initialization(main, main).
:- set_prolog_flag(double_quotes, chars).

/* sqlunit command line */
main(Args) :-
    opt_parse([[opt(table), type(atom), shortflags([t]), longflags([table])]], Args, Opts, [SqlUnit]),
    option(table(Table), Opts),
    atom(Table),
    table_sqlunit_sqlquery(Table, SqlUnit, SqlQuery),
    format('~w', SqlQuery).
main(_) :-
    halt(1).

/* sqlunit helpers */

/* DCG that always fails */
false(_,_) :- false.

/* Whitespace - Equivalent of regex \s special character */
s(C) --> [C], {member(C,[' ','\t','\n','\r'])}.
/* Whitespace - Equivalent of regex \s* */
ss --> generous(s, _).

/* Digit - Equivalent of regex \d special character */
d(C) --> [C], {member(C,"0123456789.")}.

/* Number with type - Distinguishes between numbers and percentages */
num([D|Ds], percent) --> d(D), generous(d, Ds), ss, "%".
num([D|Ds], num) --> d(D), generous(d, Ds).

/* A segment is a keyword followed by a string.  The string is matched generously (non-greedily) when parsing. Options:
     * mandatory: Disallows empty segment. Default behavior allows empty segments.
     * leadspace: Makes leading space before keyword required. */
segment(KeywordGoal, String) --> segment(KeywordGoal, String, []).
segment(_, "", Opts) --> "", {\+ option(mandatory, Opts)}.
segment(KeywordGoal, [Char|Chars], Opts) -->
    ({option(leadspace, Opts)} -> s(_); ""), ss,
    call(KeywordGoal), s(_), ss,
    [Char|Chars].

/* sqlunit syntax */
range(Num, NumType, Num, NumType) --> parsing -> false; num(Num, NumType).
range(Min, MinType, Max, MaxType) --> num(Min, MinType), ss, "-", ss, num(Max, MaxType).
range(Num, NumType, Num, NumType) --> num(Num, NumType).

unitkey(constraint) --> "".
unitkey(condition) --> "WHERE".
unitkey(group) --> "GROUP", s(_), ss, "BY".

sqlunit([SCCG1, SCCG2 | Tail]) --> sqlunit([SCCG1]), ";", sqlunit([SCCG2|Tail]).
sqlunit([sccg(range(Min, MinType, Max, MaxType), Constraint, Condition, Group)]) -->
    ss, range(Min, MinType, Max, MaxType),
    segment(unitkey(constraint), Constraint), /* no leadspace because of the space after the empty keyword */
    segment(unitkey(condition), Condition, [leadspace]),
    segment(unitkey(group), Group, [leadspace]),
    ss.

/* SQL query helpers */
discard("", _) --> "".
discard([C|Cs], Discard) --> ({member(C, Discard)} -> ""; [C]), discard(Cs, Discard).

sqlkey(where) --> "
WHERE".

testnum(Num, num, _) --> Num.
testnum(Num, percent, zero) --> Num, "/100.0*COUNT(*)".
testnum(Num, percent, minus) --> testnum(Num, percent, zero), "-0.00001".
testnum(Num, percent, plus) --> testnum(Num, percent, zero), "+0.00001".

testpass("", _) --> "COUNT(*)".
testpass([Constraint|Constraints], "") --> "SUM(CASE WHEN ", [Constraint|Constraints], " THEN 1 ELSE 0 END)".
testpass([_|_], [_|_]) --> "SUM(pass)".

testexpr(range(['1','0','0'|_], percent, ['1','0','0'|_], percent), Constraint, Group) -->
    testpass(Constraint, Group), " = COUNT(*)".
testexpr(range(Num, num, Num, num), Constraint, Group) --> testpass(Constraint, Group), " = ", Num.
testexpr(range(Num, percent, Num, percent), Constraint, Group) -->
    "ABS(", testpass(Constraint, Group), " - ", testnum(Num, percent, zero), ") < 0.00001".
testexpr(range(Min, MinType, ['1','0','0'|_], percent), Constraint, Group) -->
    testpass(Constraint, Group), " >= ", testnum(Min, MinType, minus).
testexpr(range(0, _, Max, MaxType), Constraint, Group) -->
    testpass(Constraint, Group), " <= ", testnum(Max, MaxType, plus).
testexpr(range(Min, MinType, Max, MaxType), Constraint, Group) -->
    testpass(Constraint, Group), " >= ", testnum(Min, MinType, minus),
    " AND ", testpass(Constraint, Group), " <= ", testnum(Max, MaxType, plus).

/* Expression to select the count from. */
fromexpression(Table, _, Condition, "") -->
Table,
segment(sqlkey(where), Condition).

fromexpression(Table, [Constraint|Constraints], Condition, [GroupH|GroupT]) -->
"(
  SELECT CASE WHEN ", [Constraint|Constraints], " THEN 1 ELSE 0 END AS pass
  FROM ", Table,
  segment(sqlkey(where), Condition), "
  GROUP BY ", [GroupH|GroupT], "
) g".

/* SQL query syntax - the entire test query. */
sqlquery(Table, [SCCG1, SCCG2 | Tail]) --> sqlquery(Table, [SCCG1]), "
UNION ALL
", sqlquery(Table, [SCCG2|Tail]).

sqlquery(Table, [sccg(Range, Constraint, Condition, Group)]) -->
    {phrase(sqlunit([sccg(Range, Constraint, Condition, Group)]), SqlUnit),
    phrase(discard(SqlUnit, "'"), SanitizedSqlUnit)},
"SELECT
  CASE
    WHEN ", testexpr(Range, Constraint, Group), " THEN 'PASS: ", SanitizedSqlUnit, " in ", Table, "'
    ELSE 'FAIL: ", SanitizedSqlUnit, " in ", Table, "'
  END AS test_result
FROM ",
    fromexpression(Table, Constraint, Condition, Group).

/* Relate sqlunit to SQL test query */
table_sqlunit_sqlquery(Table, SqlUnit, SqlQuery) :-
    atom_chars(Table, TableChars),
    atom_chars(SqlUnit, SqlUnitChars),
    once(phrase(sqlunit(SCCGs), SqlUnitChars)),
    once(phrase(sqlquery(TableChars, SCCGs), SqlQueryChars)),
    atom_chars(SqlQuery, SqlQueryChars).
