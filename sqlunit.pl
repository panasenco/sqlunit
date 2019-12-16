:- module(sqlunit, [table_sqlunit_sqltest/3]).

:- set_prolog_flag(double_quotes, chars).

/* sqlunit syntax (scope, constraint, condition, group) */
sqlunit(every, [CHead|CTail], "", "") --> "EVERY ", [CHead|CTail].

/* sql test query syntax (table, scope, constraint, condition, group) */
sqltest(Table, every, [CHead|CTail], "", "") -->
"SELECT
  CASE WHEN COUNT(*) = 0 THEN 'PASS' ELSE 'FAIL' END AS test_result
FROM ", Table, "
WHERE NOT(", [CHead|CTail], ")".

/* Relate sqlunit to SQL test query */
table_sqlunit_sqltest(Table, SqlUnit, SqlTest) :-
    phrase(sqlunit(Scope, Constraint, Condition, Group), SqlUnit),
    phrase(sqltest(Table, Scope, Constraint, Condition, Group), SqlTest).
