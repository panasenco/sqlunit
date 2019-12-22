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

/* sqlunit syntax */

/* Whitespace - Equivalent of regex \s special character */
s --> " ".
s --> "\t".
s --> "\n".
s --> "\r".

/* Any number of whitespace characters - equivalent to regex \s* */
ss --> "".
ss --> s, ss.

char(C) --> [C].

token(scope-every) --> "EVERY".
token(scope-some) --> "SOME".
token(condition) --> "WHERE".
token(group) --> "GROUP", s, ss, "BY".

segment(_, "", Opts) --> "", {\+ option(mandatory, Opts)}.
segment(Type, [ParamH|ParamT], Opts) -->
    ({option(first, Opts)} -> ""; s), ss,
    token(Type),
    s, ss,
    generous(char, [ParamH|ParamT]).

sqlunit([SCCG1, SCCG2 | Tail]) --> sqlunit([SCCG1]), ";", ss, sqlunit([SCCG2|Tail]).
sqlunit([sccg(Scope, Constraint, Condition, Group)]) -->
    segment(scope-Scope, Constraint, [mandatory, first]),
    segment(condition, Condition, []),
    segment(group, Group, []),
    ss.

/* SQL query syntax - helpers */
not(Condition) --> "NOT(", Condition, ")".

sanitized([],[]).
sanitized([DirtyH | DirtyT], Sanitized) :-
    DirtyH = '\''
    -> sanitized(DirtyT, Sanitized)
    ; sanitized(DirtyT, SanitizedT),
        Sanitized = [DirtyH|SanitizedT].

cleansqlunit(every, Constraint, Condition, Group) -->
    { phrase(sqlunit([sccg(every, Constraint, Condition, Group)]), Dirty), sanitized(Dirty, Sanitized)},
    Sanitized.

/* SQL query syntax - expression to select the count from. */
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
    cleansqlunit(every, Constraint, Condition, Group), " in ", Table, "'
    ELSE 'FAIL: ",
    cleansqlunit(every, Constraint, Condition, Group), " in ", Table, "'
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
