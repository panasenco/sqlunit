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

test(every_row_truepos) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    table_sqlunit_sqlquery(t, 'EVERY x IS NOT NULL', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(every_row_trueneg) :-
    table_data_create(t, [[x],[null],[2]], SqlCreate),
    table_sqlunit_sqlquery(t, 'EVERY x IS NOT NULL', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL"]).

test(every_group_truepos) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    table_sqlunit_sqlquery(t, 'EVERY COUNT(*)=1 GROUP BY x', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(every_group_trueneg) :-
    table_data_create(t, [[x],[1],[2],[1]], SqlCreate),
    table_sqlunit_sqlquery(t, 'EVERY COUNT(*)=1 GROUP BY x', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["FAIL"]).

test(sqlunit_in_result) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    table_sqlunit_sqlquery(t, 'EVERY x IS NOT NULL', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS: EVERY x IS NOT NULL"]).

test(sqlunit_in_result_sanitized) :-
    table_data_create(t, [[x],[1],[2]], SqlCreate),
    table_sqlunit_sqlquery(t, 'EVERY x <> \'abc\'', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS"]).

test(multiple) :-
    table_data_create(t, [[x],[1],[1]], SqlCreate),
    table_sqlunit_sqlquery(t, 'EVERY x IS NOT NULL; EVERY COUNT(*)=1 GROUP BY x', SqlQuery),
    atomic_concat(SqlCreate, SqlQuery, Sql),
    sql_outbegs(Sql, ["PASS","FAIL"]).
    

:- end_tests(sqlunit).
