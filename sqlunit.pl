:- set_prolog_flag(double_quotes, chars).

/* sccg = scope, constraint, condition, group */
sqlunit(sccg(every, [CHead|CTail], "", "")) --> "EVERY ", [CHead|CTail].

sql(FromExpression, sccg(every, Constraint, "", "")) --> 
"SELECT
  CASE WHEN COUNT(*) = 0 THEN 'OK' ELSE 'ERROR' END AS test_result
FROM ", FromExpression, "
WHERE NOT(", Constraint, ")".

from_sqlunit_sql(FromExpression, SqlUnit, Sql) :-
    phrase(sqlunit(STCG), SqlUnit),
    phrase(sql(FromExpression, STCG), SqlChars),
    atom_chars(Sql, SqlChars).



:- begin_tests(sqlunit).
:- set_prolog_flag(double_quotes, chars).

test(every_row) :- phrase(sqlunit(sccg(every, "x IS NOT NULL", "", "")), "EVERY x IS NOT NULL").

:- end_tests(sqlunit).
