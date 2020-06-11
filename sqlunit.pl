:- module(sqlunit, [unit_query/3]).

:- use_module(library(option)).
:- use_module(library(optparse)).

:- use_module(library(dcg_util)).

:- initialization(main, main).
:- set_prolog_flag(double_quotes, chars).

/* sqlunit command line */
main(Args) :-
    opt_parse(
        [
            [opt(table), type(atom), shortflags([t]), longflags([table])],
            [opt(style), type(atom), shortflags([s]), longflags([style])]
        ],
        Args,
        Opts,
        [SqlUnit]
    ),
    unit_query(SqlUnit, SqlQuery, Opts),
    format('~w', SqlQuery).
main(_) :-
    halt(1).

/* sqlunit helpers */

/* DCG that always fails */
false(_,_) :- false.

/* Whitespace - Equivalent of regex \s special character */
s(C) --> [C], {member(C,[' ','\t','\n','\r'])}.
s --> s(_).
/* Whitespace - Equivalent of regex \s* */
ss --> generous(s, _).

/* Digit - Equivalent of regex [\d\.] */
d(C) --> [C], {member(C,"0123456789.")}.

/* Word - Equivalent of regex [\w\.] */
w(C) --> [C], {member(C,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.")}.

/* Number with type - Distinguishes between numbers and percentages */
num([D|Ds], percent) --> d(D), generous(d, Ds), ss, "%".
num([D|Ds], num) --> d(D), generous(d, Ds).

/* A segment is a keyword followed by a string.  The string is matched generously (non-greedily) when parsing. Options:
     * mandatory: Disallows empty segment. Default behavior allows empty segments.
     * leadspace: Makes leading space before keyword required. */
segment(KeywordGoal, String) --> segment(KeywordGoal, String, []).
segment(_, "", Opts) --> "", {\+ option(mandatory, Opts)}.
segment(KeywordGoal, [Char|Chars], Opts) -->
    ({option(leadspace, Opts)} -> s; ""), ss,
    call(KeywordGoal), s, ss,
    [Char|Chars].

/* sqlunit syntax */
range(Num, NumType, Num, NumType) --> parsing -> false; num(Num, NumType).
range(Min, MinType, Max, MaxType) --> num(Min, MinType), ss, "-", ss, num(Max, MaxType).
range(Num, NumType, Num, NumType) --> num(Num, NumType).

unitkey(constraint) --> "".
unitkey(condition) --> "WHERE".
unitkey(group) --> "GROUP", s, ss, "BY".

sqlunit([SCCG1, SCCG2 | Tail]) --> sqlunit([SCCG1]), ";", sqlunit([SCCG2|Tail]).
sqlunit([sccg(range(Min, MinType, Max, MaxType), Constraint, Condition, Group)]) -->
    ss, range(Min, MinType, Max, MaxType),
    segment(unitkey(constraint), Constraint), /* no leadspace because of the space after the empty keyword */
    segment(unitkey(condition), Condition, [leadspace]),
    segment(unitkey(group), Group, [leadspace]),
    ss.

/* SQL query helpers */
discard("", _) --> "".
discard([C|Cs], Discard) --> ({member(C, Discard)} -> ""; [C]), discard(Cs, Discard).

sqlkey(where) --> sqlkey(where, 0).
sqlkey(where, IndentN) --> "
", exactly(IndentN, s), "WHERE".

/* Determine constraint type for optimization purposes */
constrainttype(inforeign(SelfKey, ForeignTable, ForeignKey)) -->
    ss, at_least(1,w,SelfKey), s, ss, "IN", s,
    ss, "(", ss, "SELECT", s, ss, at_least(1,w,ForeignKey), s, ss, "FROM", s, ss, at_least(1,w,ForeignTable), ss, ")".
constrainttype(other) --> [_|_].

testnum(Num, num, _) --> Num.
testnum(Num, percent, zero) --> Num, "/100.0*COUNT(*)".
testnum(Num, percent, minus) --> testnum(Num, percent, zero), "-0.00001".
testnum(Num, percent, plus) --> testnum(Num, percent, zero), "+0.00001".

testpass("", _) --> "COUNT(*)".
testpass([Constraint|Constraints], "") -->
    { phrase(constrainttype(inforeign(_, ForeignTable, ForeignKey)), [Constraint|Constraints]) },
    "SUM(CASE WHEN ", ForeignTable, ".", ForeignKey, " IS NOT NULL THEN 1 ELSE 0 END)".
testpass([Constraint|Constraints], "") --> "SUM(CASE WHEN ", [Constraint|Constraints], " THEN 1 ELSE 0 END)".
testpass([_|_], [_|_]) --> "SUM(pass)".

testexpr(range(['1','0','0'|_], percent, ['1','0','0'|_], percent), Constraint, Group) -->
    testpass(Constraint, Group), " = COUNT(*)".
testexpr(range(Num, num, Num, num), Constraint, Group) --> testpass(Constraint, Group), " = ", Num.
testexpr(range(Num, percent, Num, percent), Constraint, Group) -->
    "ABS(", testpass(Constraint, Group), " - ", testnum(Num, percent, zero), ") < 0.00001".
testexpr(range(Min, MinType, ['1','0','0'|_], percent), Constraint, Group) -->
    testpass(Constraint, Group), " >= ", testnum(Min, MinType, minus).
testexpr(range(0, _, Max, MaxType), Constraint, Group) -->
    testpass(Constraint, Group), " <= ", testnum(Max, MaxType, plus).
testexpr(range(Min, MinType, Max, MaxType), Constraint, Group) -->
    testpass(Constraint, Group), " >= ", testnum(Min, MinType, minus),
    " AND ", testpass(Constraint, Group), " <= ", testnum(Max, MaxType, plus).

queryoutput(Status, Message, Table, Style) -->
    {Style=simple}, ({Status = pass} -> "PASS: "; "FAIL: "), Message, " in ", Table.

queryoutput(Status, Message, Table, Style) -->
    {Style=junit},
    "<testcase name=\"", Message, " in ", Table, "\">",
    ({Status = pass} -> ""; "<failure/>"),
    "</testcase>".

queryoutput(Status, Message, Table, Style) -->
    {
        Style=nunit,
        phrase(discard(Table, "."), SanitizedTable),
        phrase(discard(Message, "."), SanitizedMessage)
    },
    "<test-case name=\"sqlunit.", SanitizedTable, ".", SanitizedMessage, "\" executed=\"True\" success=\"",
    ({Status = pass} -> "True"; "False"),
    "\"/>".

queryoutput(Status, Message, Table, Style) -->
    {Style=mocha},
    "{ \"title\": \"", Message, " in ", Table, "\", ",
    "\"state\": \"", ({Status = pass} -> "passed"; "failed"), "\" },".

/* Expression to select the count from. */
fromexpression(Table, Constraint, Condition, "") -->
{ phrase(constrainttype(inforeign(SelfKey, ForeignTable, ForeignKey)), Constraint) },
Table,"
LEFT JOIN ", ForeignTable, " ON ", Table, ".", SelfKey, " = ", ForeignTable, ".", ForeignKey,
segment(sqlkey(where), Condition).

fromexpression(Table, _, Condition, "") -->
Table,
segment(sqlkey(where), Condition).

fromexpression(Table, [Constraint|Constraints], Condition, [GroupH|GroupT]) -->
"(
  SELECT CASE WHEN ", [Constraint|Constraints], " THEN 1 ELSE 0 END AS pass
  FROM ", Table,
  segment(sqlkey(where,2), Condition), "
  GROUP BY ", [GroupH|GroupT], "
) g".

/* SQL query syntax - the entire test query. */
sqlquery([SCCG1, SCCG2 | Tail], Opts) --> sqlquery([SCCG1], Opts), "
UNION ALL
", sqlquery([SCCG2|Tail], Opts).

sqlquery([sccg(Range, Constraint, Condition, Group)], Opts) -->
    {
        phrase(sqlunit([sccg(Range, Constraint, Condition, Group)]), SqlUnit),
        phrase(discard(SqlUnit, "'"), SanitizedSqlUnit),
        option(table(TableAtom), Opts),
        atom(TableAtom),
        atom_chars(TableAtom, Table),
        option(style(Style), Opts, simple)
    },
"SELECT
  CASE
    WHEN ", testexpr(Range, Constraint, Group), " THEN '", queryoutput(pass, SanitizedSqlUnit, Table, Style), "'
    ELSE '", queryoutput(fail, SanitizedSqlUnit, Table, Style), "'
  END AS test_result
FROM ",
    fromexpression(Table, Constraint, Condition, Group).

/* Relate sqlunit to SQL test query */
unit_query(SqlUnit, SqlQuery, Opts) :-
    atom_chars(SqlUnit, SqlUnitChars),
    once(phrase(sqlunit(SCCGs), SqlUnitChars)),
    once(phrase(sqlquery(SCCGs, Opts), SqlQueryChars)),
    atom_chars(SqlQuery, SqlQueryChars).
