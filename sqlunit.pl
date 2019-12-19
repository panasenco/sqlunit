:- module(sqlunit, [table_sqlunit_sqlquery/3]).

:- set_prolog_flag(double_quotes, chars).

/* sqlunit syntax */
sqlunit(every, [ConstraintH|ConstraintT], "", [GroupH|GroupT]) -->
  "EVERY ", [ConstraintH|ConstraintT],
  " GROUP BY ", [GroupH|GroupT].
sqlunit(every, [ConstraintH|ConstraintT], "", "") -->
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
    { phrase(sqlunit(every, Constraint, Condition, Group), Dirty), sanitized(Dirty, Sanitized)},
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
sqlquery(Table, every, Constraint, Condition, Group) -->
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
    once(phrase(sqlunit(Scope, Constraint, Condition, Group), SqlUnit)),
    once(phrase(sqlquery(Table, Scope, Constraint, Condition, Group), SqlQuery)).
