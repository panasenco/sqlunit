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
    tmp_file_stream(File, Stream, []),
    write(Stream, Sql),
    close(Stream),
    atom_concat('.read ', File, SqlArg),
    process_create(path(sqlite3), [':memory:', SqlArg], [stdout(pipe(Out))]),
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
uq(SqlUnit, SqlQuery, Opts) :-
    format('~n~`-t~120|~n~w~n~`-t~120|', [SqlUnit]),
    call_with_inference_limit(unit_query(SqlUnit, SqlQuery, Opts), 999999, '!'),
    format('~n~w~n', [SqlQuery]).

test(every_row_truepos) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    uq('100% x IS NOT NULL', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(every_row_trueneg) :-
    table_data_create(t, [[x],[null],[2]], SqlCreate),
    uq('100% x IS NOT NULL', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL"]).

test(every_group_truepos) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    uq('100% COUNT(*)=1 GROUP BY x', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(every_group_trueneg) :-
    table_data_create(t, [[x],[1],[2],[1]], SqlCreate),
    uq('100% COUNT(*)=1 GROUP BY x', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL"]).

test(sqlunit_in_result) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    uq('100% x IS NOT NULL', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS: 100% x IS NOT NULL"]).

test(sqlunit_in_result_sanitized) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    uq('100% x <> \'abc\'', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(multiple) :-
    table_data_create(t, [[x],[1],[1]], SqlCreate),
    uq('100% x IS NOT NULL; 100% COUNT(*)=1 GROUP BY x; 100% x=1', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS"]).

test(whitespace) :-
    table_data_create(t, [[x],[1],[1]], SqlCreate),
    uq('    100     % 	 x  IS NOT	 NULL  ;
           100
      %  COUNT(*)=1    GROUP	BY  x   ', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL"]).

test(some) :-
    table_data_create(t, [[x],[1],[2],[2],[3]], SqlCreate),
    uq('1-100% x = 2; 1-100% x=4; 1-100% COUNT(*) = 2 GROUP BY x; 1-100% COUNT(*) = 0 GROUP BY x', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS","FAIL"]).

test(condition) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[null,2]], SqlCreate),
    uq('100% x IS NOT NULL WHERE y=1; 1-100% x = \'a\' WHERE y = 1; 100% COUNT(*) = 1 WHERE y=1 GROUP BY x; 1-100% COUNT(*) = 2 WHERE y IS NOT NULL GROUP BY x', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS","FAIL"]).

test(range) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[null,2]], SqlCreate),
    uq(' 2 -3 x=2; 0- 1 x=2;   0-0   x IS NOT NULL; 0-1 x=2 WHERE y IS NOT NULL', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","FAIL","PASS"]).

test(rangepercent) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[null,2]], SqlCreate),
    uq(' 62.5%-80.1% x IS NOT NULL;  40 % - 60 %  x=2 WHERE y IS NOT NULL;   0-100%   x = 4; 50%-50% x=1 WHERE y=1', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL","PASS","PASS"]).

test(rangegroup) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    uq('1-2 COUNT(*)=2 GROUP BY y; 0-50% COUNT(*)=1 GROUP BY x; 40%-50% COUNT(*)=2 GROUP BY y', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","PASS","FAIL"]).

test(rowcount) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    uq('4; 3-5; 100%; 0-3', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","PASS","PASS","FAIL"]).

test(rangeedge) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    uq('2 x=2; 1 y IS NULL; 25% x=1; 50% y=2; 0-100% x=99; 0-72% y IS NOT NULL; 0-2 x=1; 3-99% x IS NOT NULL;
        2-75% x=2', SqlQuery, [table(t)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL","PASS","PASS","FAIL","PASS","FAIL","PASS","FAIL","PASS"]).

test(inforeign) :-
    table_data_create('aux.t1', [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate1),
    table_data_create(t2, [[x],[1],[null],[2]], SqlCreate2),
    atomic_concat(SqlCreate1, SqlCreate2, SqlCreate3),
    atomic_concat('ATTACH DATABASE \':memory:\' AS aux;\n', SqlCreate3, SqlCreate),
    uq('100% x IN (SELECT x FROM aux.t1); 100% x IN (SELECT y FROM t1);
        100% x IN (SELECT y FROM aux.t1) WHERE t2.x IS NOT NULL; 60%-100% x IN (SELECT x FROM aux.t1)',
        SqlQuery, [table(t2)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL","FAIL","PASS","PASS"]).
    
test(inforeign_optimized) :-
    table_data_create('aux.t1', [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate1),
    table_data_create(t2, [[x],[1],[null],[2]], SqlCreate2),
    atomic_concat(SqlCreate1, SqlCreate2, SqlCreate3),
    atomic_concat('ATTACH DATABASE \':memory:\' AS aux;\n', SqlCreate3, SqlCreate),
    uq('100% x IN (SELECT y FROM aux.t1)', SqlQuery, [table(t2)]),
    once(sub_atom(SqlQuery, _, _, _, 'WHEN SUM(CASE WHEN aux.t1.y IS NOT NULL THEN 1 ELSE 0 END) = COUNT(*)')),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL"]).

test(junit_simple) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    uq('2 x=2; 1 y IS NULL', SqlQuery, [table(t), style(junit)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["<testcase","<testcase"]).

test(nunit_simple) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    uq('2 x=2; 1 y IS NULL', SqlQuery, [table(t), style(junit)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["<test-case","<test-case"]).

test(mocha_simple) :-
    table_data_create(t, [[x,y],[1,1],[2,1],[2,null],[2,2]], SqlCreate),
    uq('2 x=2; 1 y IS NULL', SqlQuery, [table(t), style(mocha)]),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["{ \"title","{ \"title"]).

:- end_tests(sqlunit).
