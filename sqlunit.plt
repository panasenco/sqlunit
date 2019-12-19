:- set_prolog_flag(double_quotes, chars).

/* Helper predicates for below tests */
chars(Atom, Chars, Rest) :-
    atom_chars(Atom, AtomChars),
    append(AtomChars, Rest, Chars).

commaseparated(List) --> commaseparated(List, "").
commaseparated([H | []], Suffix) --> chars(H), Suffix.
commaseparated([H | [TH | TT]], Suffix) --> commaseparated([H], Suffix), ", ", commaseparated([TH | TT], Suffix).

insert(_, []) --> "".
insert(Table, [DataH | DataT]) -->
    "INSERT INTO ", Table, " VALUES(", commaseparated(DataH), "); ", insert(Table, DataT).

sqldata(Table, [[HeaderH | HeaderT] | Data]) -->
    "DROP TABLE IF EXISTS ", Table,
    "; CREATE TABLE ", Table, "(", commaseparated([HeaderH | HeaderT], " smallint"), ");",
    insert(Table, Data).

/* Relate sqlite SQL query with the beginning of its output.*/
sql_outstart(Sql, OutStart) :-
    atom_chars(SqlAtom, Sql),
    process_create(path(sqlite3), [':memory:', SqlAtom], [stdout(pipe(Out))]),
    read_line_to_string(Out, OutAtom),
    atom_chars(OutAtom, OutChars),
    append(OutStart, _, OutChars).

/* End-to-end tests */
:- begin_tests(sqlunit).
:- use_module(sqlunit).
:- set_prolog_flag(double_quotes, chars).

test(every_row_truepos) :-
    once(phrase(sqldata("t", [[x],[1],[2]]), Data)),
    table_sqlunit_sqlquery("t", "EVERY x IS NOT NULL", SqlQuery),
    append(Data, SqlQuery, Sql),
    sql_outstart(Sql, "PASS").

test(every_row_trueneg) :-
    once(phrase(sqldata("t", [[x],[null],[2]]), Data)),
    table_sqlunit_sqlquery("t", "EVERY x IS NOT NULL", SqlQuery),
    append(Data, SqlQuery, Sql),
    sql_outstart(Sql, "FAIL").

test(every_group_truepos) :-
    once(phrase(sqldata("t", [[x],[1],[2]]), Data)),
    table_sqlunit_sqlquery("t", "EVERY COUNT(*)=1 GROUP BY x", SqlQuery),
    append(Data, SqlQuery, Sql),
    sql_outstart(Sql, "PASS").

test(every_group_trueneg) :-
    once(phrase(sqldata("t", [[x],[1],[2],[1]]), Data)),
    table_sqlunit_sqlquery("t", "EVERY COUNT(*)=1 GROUP BY x", SqlQuery),
    append(Data, SqlQuery, Sql),
    sql_outstart(Sql, "FAIL").

test(sqlunit_in_result) :-
    once(phrase(sqldata("t", [[x],[1],[2]]), Data)),
    table_sqlunit_sqlquery("t", "EVERY x IS NOT NULL", SqlQuery),
    append(Data, SqlQuery, Sql),
    sql_outstart(Sql, "PASS: EVERY x IS NOT NULL").

test(sqlunit_in_result_sanitized) :-
    once(phrase(sqldata("t", [[x],[1],[2]]), Data)),
    table_sqlunit_sqlquery("t", "EVERY x <> 'abc'", SqlQuery),
    append(Data, SqlQuery, Sql),
    sql_outstart(Sql, "PASS").

:- end_tests(sqlunit).
