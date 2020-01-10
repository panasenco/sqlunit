:- set_prolog_flag(double_quotes, chars).

/* Define SQL create/insert statement in a concise and dirty way using atom concatenation rather than DCGs */
table_values_insert(_, [], '').
table_values_insert(Table, [[ValuesHH|ValuesHT] | ValuesT], SqlInsert) :-
    atomic_list_concat([ValuesHH|ValuesHT], ', ', ValuesAtom),
    atomic_list_concat(['INSERT INTO ', Table, ' VALUES(', ValuesAtom, '); '], SqlInsertRow),
    once(table_values_insert(Table, ValuesT, SqlInsertRest)),
    atomic_concat(SqlInsertRow, SqlInsertRest, SqlInsert).

table_data_create(Table, [[HeaderH|HeaderT] | Values], SqlCreate) :-
    atomic_list_concat([HeaderH|HeaderT], ', ', HeaderAtom),
    atomic_list_concat(['DROP TABLE IF EXISTS ', Table, '; CREATE TABLE ', Table, '(', HeaderAtom, ');'], SqlHeader),
    table_values_insert(Table, Values, SqlInsert),
    atomic_concat(SqlHeader, SqlInsert, SqlCreate).

/* Relate sqlite SQL query with its output.*/
sql_outchars(Sql, OutChars) :-
    process_create(path(sqlite3), [':memory:', Sql], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, OutCodes),
    atom_codes(OutAtom, OutCodes),
    format('~n~`.t~120|~n~w~`.t~120|~n', [OutAtom]),
    atom_chars(OutAtom, OutChars).

/* Relate charlist with list of lists of first characters of its lines */
chars_linebegs(_, []).
chars_linebegs(Chars, [[LineBegHH|LineBegHT]|LineBegT]) :-
    append([LineBegHH|LineBegHT], Remainder, Chars),
    append(_, ['\n' | FollowingLines], Remainder),
    chars_linebegs(FollowingLines, LineBegT).

/* Relate sqlite SQL query with first characters of its output lines */
sql_outbegs(Sql, OutBegs) :-
    sql_outchars(Sql, OutChars),
    once(chars_linebegs(OutChars, OutBegs)).


/* End-to-end tests */
:- begin_tests(sqlunit).
:- use_module(sqlunit).
:- set_prolog_flag(double_quotes, chars).

/* Put inference limit on the tests */
tss(Table, SqlUnit, SqlQuery) :-
    format('~n~`-t~120|~n~w~n~`-t~120|', [SqlUnit]),
    call_with_inference_limit(table_sqlunit_sqlquery(Table, SqlUnit, SqlQuery), 999999, '!'),
    format('~n~w~n', [SqlQuery]).

test(every_row_truepos) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    tss(t, '100% x IS NOT NULL', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(every_row_trueneg) :-
    table_data_create(t, [[x],[null],[2]], SqlCreate),
    tss(t, '100% x IS NOT NULL', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL"]).

test(every_group_truepos) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    tss(t, '100% COUNT(*)=1 GROUP BY x', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(every_group_trueneg) :-
    table_data_create(t, [[x],[1],[2],[1]], SqlCreate),
    tss(t, '100% COUNT(*)=1 GROUP BY x', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL"]).

test(sqlunit_in_result) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    tss(t, '100% x IS NOT NULL', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS: 100% x IS NOT NULL"]).

test(sqlunit_in_result_sanitized) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    tss(t, '100% x <> \'abc\'', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(multiple) :-
    table_data_create(t, [[x],[1],[1]], SqlCreate),
    tss(t, '100% x IS NOT NULL; 100% COUNT(*)=1 GROUP BY x; 100% x=1', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS"]).

test(whitespace) :-
    table_data_create(t, [[x],[1],[1]], SqlCreate),
    tss(t, '    100     % 	 x  IS NOT	 NULL  ;
           100
      %  COUNT(*)=1    GROUP	BY  x   ', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL"]).

test(some) :-
    table_data_create(t, [[x],[1],[2],[2],[3]], SqlCreate),
    tss(t, '1-100% x = 2; 1-100% x=4; 1-100% COUNT(*) = 2 GROUP BY x; 1-100% COUNT(*) = 0 GROUP BY x', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS","FAIL"]).

test(condition) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[null,2]], SqlCreate),
    tss(t, '100% x IS NOT NULL WHERE y=1; 1-100% x = \'a\' WHERE y = 1; 100% COUNT(*) = 1 WHERE y=1 GROUP BY x; 1-100% COUNT(*) = 2 WHERE y IS NOT NULL GROUP BY x', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS","FAIL"]).

test(range) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[null,2]], SqlCreate),
    tss(t, ' 2 -3 x=2; 0- 1 x=2;   0-0   x IS NOT NULL; 0-1 x=2 WHERE y IS NOT NULL', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","FAIL","PASS"]).

test(rangepercent) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[null,2]], SqlCreate),
    tss(t, ' 62.5%-80.1% x IS NOT NULL;  40 % - 60 %  x=2 WHERE y IS NOT NULL;   0-100%   x = 4; 50%-50% x=1 WHERE y=1', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS","PASS"]).

test(rangegroup) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    tss(t, '1-2 COUNT(*)=2 GROUP BY y; 0-50% COUNT(*)=1 GROUP BY x; 40%-50% COUNT(*)=2 GROUP BY y', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","PASS","FAIL"]).

test(rowcount) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    tss(t, '4; 3-5; 100%; 0-3', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","PASS","PASS","FAIL"]).

test(rangeedge) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    tss(t, '2 x=2; 1 y IS NULL; 25% x=1; 50% y=2; 0-100% x=99; 0-72% y IS NOT NULL; 0-2 x=1; 3-99% x IS NOT NULL; 2-75% x=2', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL","PASS","PASS","FAIL","PASS","FAIL","PASS","FAIL","PASS"]).

:- end_tests(sqlunit).
