:- module(sqlunit, [table_sqlunit_sqltest/3]).

:- set_prolog_flag(double_quotes, chars).

/* sqlunit syntax (scope, constraint, condition, group) */
sqlunit(every, [ConstraintH|ConstraintT], "", [GroupH|GroupT]) -->
    "EVERY ", [ConstraintH|ConstraintT],
    " GROUP BY ", [GroupH|GroupT].
sqlunit(every, [ConstraintH|ConstraintT], "", "") --> "EVERY ", [ConstraintH|ConstraintT].

/* sql test query syntax (table, scope, constraint, condition, group) */
selectresult --> "SELECT
  CASE WHEN COUNT(*) = 0 THEN 'PASS' ELSE 'FAIL' END AS test_result
FROM ".

where --> "
WHERE ".

not(Condition) --> "NOT(", Condition, ")".

groupselect(Table, Constraint, Group) -->
"(
  SELECT 1 AS dummy
  FROM ", Table, "
  GROUP BY ", Group, "
  HAVING ", not(Constraint), "
)".

sqltest(Table, every, [ConstraintH|ConstraintT], "", "") --> selectresult, Table, where, not([ConstraintH|ConstraintT]).
sqltest(Table, every, [ConstraintH|ConstraintT], "", [GroupH|GroupT]) --> selectresult, groupselect(Table, [ConstraintH|ConstraintT], [GroupH|GroupT]).

/* Relate sqlunit to SQL test query */
table_sqlunit_sqltest(Table, SqlUnit, SqlTest) :-
    once(phrase(sqlunit(Scope, Constraint, Condition, Group), SqlUnit)),
    once(phrase(sqltest(Table, Scope, Constraint, Condition, Group), SqlTest)).
