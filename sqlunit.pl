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
s --> " ".
s --> "\t".
s --> "\n".
s --> "\r".

/* Any number of whitespace characters - equivalent to regex \s* */
ss --> "".
ss --> s, ss.

generous_discard("", _) --> "".
generous_discard([C|Cs], Discard) --> ({member(C, Discard)} -> ""; [C]), generous_discard(Cs, Discard).

token(scope-every) --> "EVERY".
token(scope-some) --> "SOME".
token(condition) --> "WHERE".
token(group) --> "GROUP", s, ss, "BY".

segment(_, "", Opts) --> "", {\+ option(mandatory, Opts)}.
segment(Type, [ParamH|ParamT], Opts) -->
    ({option(first, Opts)} -> ""; s), ss,
    token(Type),
    s, ss,
    {option(discard(Discard), Opts, "")},
    generous_discard([ParamH|ParamT], Discard).

/* sqlunit syntax */
sqlunit(SCCGs) --> sqlunit(SCCGs, []).
sqlunit([SCCG1, SCCG2 | Tail], Opts) --> sqlunit([SCCG1], Opts), ";", ss, sqlunit([SCCG2|Tail], Opts).
sqlunit([sccg(Scope, Constraint, Condition, Group)], Opts) -->
    segment(scope-Scope, Constraint, [mandatory, first | Opts]),
    segment(condition, Condition, Opts),
    segment(group, Group, Opts),
    ss.

/* SQL query helpers */
not(Condition) --> "NOT(", Condition, ")".

/* Expression to select the count from. */
fromexpression(Table, [ConstraintH|ConstraintT], "", "") -->
Table, "
WHERE ",
not([ConstraintH|ConstraintT]).

fromexpression(Table, [ConstraintH|ConstraintT], "", [GroupH|GroupT]) -->
"(
  SELECT 1 AS dummy
  FROM ", Table, "
  GROUP BY ", [GroupH|GroupT], "
  HAVING ", not([ConstraintH|ConstraintT]), "
) g".

/* SQL query syntax - the entire test query. */
sqlquery(Table, [SCCG1, SCCG2 | Tail]) --> sqlquery(Table, [SCCG1]), "
UNION ALL
", sqlquery(Table, [SCCG2|Tail]).

sqlquery(Table, [sccg(every, Constraint, Condition, Group)]) -->
"SELECT
  CASE
    WHEN COUNT(*) = 0 THEN 'PASS: ",
    sqlunit([sccg(every, Constraint, Condition, Group)], [discard("'")]), " in ", Table, "'
    ELSE 'FAIL: ",
    sqlunit([sccg(every, Constraint, Condition, Group)], [discard("'")]), " in ", Table, "'
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
