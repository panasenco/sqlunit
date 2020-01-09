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

/* Optional - Equivalent of regex + */
o(_) --> "".
o(C) --> C.

/* Whitespace - Equivalent of regex \s special character */
s(C) --> [C], {member(C,[' ','\t','\n','\r'])}.
/* Whitespace - Equivalent of regex \s* */
ss --> generous(s, _).

/* Digit - Equivalent of regex \d special character */
d(C) --> [C], {member(C,"0123456789.")}.

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
unitkey(scope-every) --> "EVERY".
unitkey(scope-some) --> "SOME".
unitkey(scope-range([Min|Mins], [Max|Maxs], count)) --> "RANGE", ss, "(", ss, d(Min), generous(d,Mins), ss, "-", ss, d(Max), generous(d, Maxs), ss, ")".
unitkey(scope-range([Min|Mins], [Max|Maxs], percent)) --> "RANGE", ss, "(", ss, d(Min), generous(d,Mins), ss, o("%"), ss, "-", ss, d(Max), generous(d, Maxs), ss, "%", ss, ")".
unitkey(condition) --> "WHERE".
unitkey(group) --> "GROUP", s(_), ss, "BY".

sqlunit([SCCG1, SCCG2 | Tail]) --> sqlunit([SCCG1]), ";", sqlunit([SCCG2|Tail]).
sqlunit([sccg(Scope, Constraint, Condition, Group)]) -->
    segment(unitkey(scope-Scope), Constraint, [mandatory]),
    segment(unitkey(condition), Condition, [leadspace]),
    segment(unitkey(group), Group, [leadspace]),
    ss.

/* SQL query helpers */
discard("", _) --> "".
discard([C|Cs], Discard) --> ({member(C, Discard)} -> ""; [C]), discard(Cs, Discard).

sqlkey(where) --> "
WHERE".
sqlkey(and) --> "AND".

test(every, Condition) --> "NOT(", Condition, ")".
test(some, Condition) --> Condition.
test(range(_,_,count), Condition) --> Condition.
test(range(_,_,percent), _) --> "".

testexpr(every, _) --> "COUNT(*) = 0".
testexpr(some, _) --> "COUNT(*) >= 1".
testexpr(range(Min, Max, count), _) --> "COUNT(*) >= ", Min, " AND COUNT(*) <= ", Max.
testexpr(range(Min, Max, percent), Constraint) --> "SUM(CASE WHEN ", Constraint, " THEN 1 ELSE 0 END)*100.0/COUNT(*) >= ", Min, " AND SUM(CASE WHEN ", Constraint, " THEN 1 ELSE 0 END)*100.0/COUNT(*) <= ", Max.

/* Expression to select the count from. */
fromexpression(Scope, Table, [ConstraintH|ConstraintT], Condition, "") -->
Table,
{phrase(test(Scope, [ConstraintH|ConstraintT]), Test)},
segment(sqlkey(where), Test),
{Test="" -> CondKey=where; CondKey=and},
segment(sqlkey(CondKey), Condition, [leadspace]).

fromexpression(Scope, Table, [ConstraintH|ConstraintT], Condition, [GroupH|GroupT]) -->
"(
  SELECT 1 AS dummy
  FROM ", Table,
  segment(sqlkey(where), Condition), "
  GROUP BY ", [GroupH|GroupT], "
  HAVING ", test(Scope, [ConstraintH|ConstraintT]), "
) g".

/* SQL query syntax - the entire test query. */
sqlquery(Table, [SCCG1, SCCG2 | Tail]) --> sqlquery(Table, [SCCG1]), "
UNION ALL
", sqlquery(Table, [SCCG2|Tail]).

sqlquery(Table, [sccg(Scope, Constraint, Condition, Group)]) -->
{once(phrase(sqlunit([sccg(Scope, Constraint, Condition, Group)]), SqlUnit)), phrase(discard(SqlUnit, "'"), SanitizedSqlUnit)},
"SELECT
  CASE
    WHEN ", testexpr(Scope, Constraint), " THEN 'PASS: ", SanitizedSqlUnit, " in ", Table, "'
    ELSE 'FAIL: ", SanitizedSqlUnit, " in ", Table, "'
  END AS test_result
FROM ",
    fromexpression(Scope, Table, Constraint, Condition, Group).

/* Relate sqlunit to SQL test query */
table_sqlunit_sqlquery(Table, SqlUnit, SqlQuery) :-
    atom_chars(Table, TableChars),
    atom_chars(SqlUnit, SqlUnitChars),
    once(phrase(sqlunit(SCCGs), SqlUnitChars)),
    once(phrase(sqlquery(TableChars, SCCGs), SqlQueryChars)),
    atom_chars(SqlQuery, SqlQueryChars).
