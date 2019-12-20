:- module(sqlunit, [table_sqlunit_sqlquery/3]).

:- use_module(library(option)).
:- use_module(library(optparse)).

:- initialization(main, main).
:- set_prolog_flag(double_quotes, chars).

/* sqlunit script */
main(Args) :-
    opt_parse([[opt(table), type(atom), shortflags([t]), longflags([table])]], Args, Opts, [SqlUnit]),
    option(table(Table), Opts),
    atom(Table),
    table_sqlunit_sqlquery(Table, SqlUnit, SqlQuery),
    format('~w', SqlQuery).
main(_) :-
    halt(1).

/* sqlunit syntax (sccg = scope constraint condition group) */
sqlunit([SCCG1, SCCG2 | Tail]) --> sqlunit([SCCG1]), "; ", sqlunit([SCCG2|Tail]).
sqlunit([sccg(every, [ConstraintH|ConstraintT], "", [GroupH|GroupT])]) -->
  "EVERY ", [ConstraintH|ConstraintT],
  " GROUP BY ", [GroupH|GroupT].
sqlunit([sccg(every, [ConstraintH|ConstraintT], "", "")]) -->
  "EVERY ", [ConstraintH|ConstraintT].

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
)".

/* SQL query syntax - the entire test query. */
sqlquery(Table, [SCCG1, SCCG2 | Tail]) --> sqlquery(Table, [SCCG1]), "
UNION ALL
", sqlquery(Table, [SCCG2|Tail]).

sqlquery(Table, [sccg(every, Constraint, Condition, Group)]) -->
"SELECT
  CASE
    WHEN COUNT(*) = 0 THEN 'PASS: ",
    cleansqlunit(every, Constraint, Condition, Group), ".'
    ELSE 'FAIL: ",
    cleansqlunit(every, Constraint, Condition, Group),
  ".'
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
