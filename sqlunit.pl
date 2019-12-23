:- module(sqlunit, [table_sqlunit_sqlquery/3]).

:- use_module(library(dcg_util)).
:- use_module(library(option)).
:- use_module(library(optparse)).

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

/* Whitespace - Equivalent of regex \s special character */
s(C) --> [C], {member(C,[' ','\t','\n','\r'])}.

/* A segment is a leading space followed by a keyword followed by a string.
   The string is matched generously (non-greedily) when parsing. Options:
     * mandatory: Disallows empty segment. Default behavior allows empty segments.
     * noleadspace: Makes the leading space optional. Default behavior is required lead space.
     * discard(List): Discards characters in the given list when generating (not parsing) strings. */

generous_discard("", _) --> "".
generous_discard([C|Cs], Discard) --> ({member(C, Discard)} -> ""; [C]), generous_discard(Cs, Discard).

segment(KeywordGoal, String) --> segment(KeywordGoal, String, []).
segment(_, "", Opts) --> "", {\+ option(mandatory, Opts)}.
segment(KeywordGoal, [Char|Chars], Opts) -->
    ({option(noleadspace, Opts)} -> ""; s(_)), generous(s, _),
    call(KeywordGoal),
    s(_), generous(s, _),
    {option(discard(Discard), Opts, [])},
    generous_discard([Char|Chars], Discard).

/* sqlunit syntax */
unitkey(scope-every) --> "EVERY".
unitkey(scope-some) --> "SOME".
unitkey(condition) --> "WHERE".
unitkey(group) --> "GROUP", s(_), generous(s, _), "BY".

sqlunit(SCCGs) --> sqlunit(SCCGs, []).
sqlunit([SCCG1, SCCG2 | Tail], Opts) --> sqlunit([SCCG1], Opts), ";", sqlunit([SCCG2|Tail], Opts).
sqlunit([sccg(Scope, Constraint, Condition, Group)], Opts) -->
    segment(unitkey(scope-Scope), Constraint, [mandatory, noleadspace | Opts]),
    segment(unitkey(condition), Condition, Opts),
    segment(unitkey(group), Group, Opts),
    generous(s, _).

/* SQL query helpers */
sqlkey(and) --> "AND".
sqlkey(where) --> "
WHERE".

test(every, Condition) --> "NOT(", Condition, ")".
test(some, Condition) --> Condition.

countcomparison(every) --> " = 0".
countcomparison(some) --> " >= 1".

/* Expression to select the count from. */
fromexpression(Scope, Table, [ConstraintH|ConstraintT], Condition, "") -->
Table, "
WHERE ",
test(Scope, [ConstraintH|ConstraintT]),
segment(sqlkey(and), Condition).

fromexpression(Scope, Table, [ConstraintH|ConstraintT], Condition, [GroupH|GroupT]) -->
"(
  SELECT 1 AS dummy
  FROM ", Table,
  segment(sqlkey(where), Condition, [noleadspace]), "
  GROUP BY ", [GroupH|GroupT], "
  HAVING ", test(Scope, [ConstraintH|ConstraintT]), "
) g".

/* SQL query syntax - the entire test query. */
sqlquery(Table, [SCCG1, SCCG2 | Tail]) --> sqlquery(Table, [SCCG1]), "
UNION ALL
", sqlquery(Table, [SCCG2|Tail]).

sqlquery(Table, [sccg(Scope, Constraint, Condition, Group)]) -->
"SELECT
  CASE
    WHEN COUNT(*)", countcomparison(Scope), " THEN 'PASS: ",
    sqlunit([sccg(Scope, Constraint, Condition, Group)], [discard(['\''])]), " in ", Table, "'
    ELSE 'FAIL: ",
    sqlunit([sccg(Scope, Constraint, Condition, Group)], [discard(['\''])]), " in ", Table, "'
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
