-module(dna2rna).
-export([execute/0, execute/2, pattern/1, asnat/1, test1/0, test2/0, test3/0]).

%%% turns into RNA: PICFC
test1() ->
    execute(['I', 'I', 'P', 'I', 'P', 'I', 'C', 'P', 'I', 'I', 'C', 'I', 'C', 'I', 'I', 'F', 'I', 'C', 'C', 'I', 'F', 'P', 'P', 'I', 'I', 'C', 'C', 'F', 'P', 'C'], []).

%%% turns into RNA: PIICCFCFFPC
test2() ->
    execute(['I','I','P','I','P','I','C','P','I','I','C','I','C','I','I','F','I','C','C','I','F','C','C','C','P','P','I','I','C','C','F','P','C'], []).

%%% turns into RNA: I
test3() ->
    execute(['I','I','P','I','P','I','I','C','P','I','I','C','I','I','C','C','I','I','C','F','C','F','C'], []).

execute() ->
    case file:read_file("endo.dna") of
	{ok, Binary} ->
	    DNA = binary_to_list(Binary),
	    execute(DNA, []);
	_ ->
	    io:format("ERROR reading dna~n", [])
    end.

execute([], []) -> error;
execute(DNA, RNA) ->
    {NewDNA, RNA, P} = pattern(DNA, [], [], 0),
    {NewDNA2, NewRNA, T} = template(NewDNA, RNA, []),
    io:format("DNA: ~w~nP : ~w~nT : ~w~n", [NewDNA2, P, T]), 
    NewDNA3 = matchreplace(P, T, NewDNA2),
    io:format("DNA3 : ~w~n", [NewDNA3]), 
    execute(NewDNA3, NewRNA).

finish(RNA) ->
    io:format("RNA : ~w~n", [RNA]),
    halt().

pattern(DNA) ->
    pattern(DNA, [], [], 0).

pattern(['C' | T], RNA, P, LVL) ->
    pattern(T, RNA, P++['I'], LVL);
pattern(['F' | T], RNA, P, LVL) ->
    pattern(T, RNA, P++['C'], LVL);
pattern(['P' | T], RNA, P, LVL) ->
    pattern(T, RNA, P++['F'], LVL);
pattern(['I', 'C' | T], RNA, P, LVL) ->
    pattern(T, RNA, P++['P'], LVL);
pattern(['I', 'P' | T], RNA, P, LVL) ->
    {DNA, N} = nat(T, RNA),
    A = "!"++[N],
    pattern(DNA, RNA, P++[A], LVL);
pattern(['I', 'F' | T], RNA, P, LVL) ->
    {DNA, C} = consts(T),
     A = "?"++[C],
    pattern(lists:nthtail(1, DNA), RNA, P++[A], LVL);
pattern(['I', 'I', 'P' | T], RNA, P, LVL) ->
    pattern(T, RNA, P++['('], LVL+1);
pattern(['I', 'I', 'C' | T], RNA, P, LVL) ->
    pattern(iicORiif, T, RNA, P, LVL);
pattern(['I', 'I', 'F' | T], RNA, P, LVL) ->
    pattern(iicORiif, T, RNA, P, LVL);
pattern(['I', 'I', 'I' | T], RNA, P, LVL) ->
    pattern(lists:nthtail(10, T), RNA++[lists:sublist(T, 3, 10)], P, LVL);
pattern(_, RNA, P, _) ->
    io:format("Pattern : ~w~n", [P]),
    finish(RNA).

pattern(iicORiif, T, RNA, P, LVL) ->
    if 
	LVL == 0 ->
	    {T, RNA, P};
	true ->
	    pattern(T, RNA, P++[')'], LVL-1)
    end.

nat(['P' | T], _) ->
    {T, 0};
nat(['I' | T], RNA) ->
    {DNA, N} = nat(T, RNA),
    {DNA, 2*N};
nat(['F' | T], RNA) ->
    {DNA, N} = nat(T, RNA),
    {DNA, 2*N};      
nat(['C' | T], RNA) ->
    {DNA, N} = nat(T, RNA),
    {DNA, (2*N) + 1};
nat([], RNA) ->
    finish(RNA).


consts(['C' | T]) ->
    {T, ['I']++[consts(T)]};
consts(['F' | T]) ->
    {T, ['C']++[consts(T)]};
consts(['P' | T]) ->
    {T, ['F']++[consts(T)]};
consts(['I', 'C' | T]) ->
    {T, ['P']++[consts(T)]};
consts(DNA) ->
    {DNA, []}.

matchreplace(Pattern, Template, DNA) ->
    {I, E, NewDNA2} = pattern_loop(Pattern, 1, [], [], DNA),
    io:format("MATCHREPLACE DNA: ~w ~w~n", [NewDNA2, I]),
    if 
	I > 0 ->
	    NewDNA3 = lists:nthtail(I, NewDNA2),
	    {_, NewDNA4, R} = replace(Template, E, NewDNA3, []),
	    R++NewDNA4;
	true ->
	    NewDNA2
    end.

replace([], E, DNA, R) ->
    {E, DNA, R};
replace(['C' | T], E, DNA, R) ->
    replace(T, E, DNA, R++['C']);
replace(['F' | T], E, DNA, R) ->
    replace(T, E, DNA, R++['F']);
replace(['P' | T], E, DNA, R) ->
    replace(T, E, DNA, R++['P']);
replace(['I' | T], E, DNA, R) ->    
    replace(T, E, DNA, R++['I']);
replace([[N, '_', L] | T], E, DNA, R) ->
    if 
	N == 0 ->
	    Append = protect(L, []),
	    io:format("APPEND: ~w~n", [Append]),
	    replace(T, E, DNA, R++Append);
	true ->
	    Append = protect(L, lists:nth(N, E)),
	io:format("APPEND: ~w~n", [Append]),	
	    replace(T, E, DNA, R++Append)
    end;
replace(['|', N, '|' | T], E, DNA, R) ->
    if 
	N == 0 ->
	    replace(T, E, DNA, R++asnat(length([])));
	true ->
	    replace(T, E, DNA, R++asnat(length(lists:nth(N, E))))
    end.

protect(L, DNA) when L == 0 ->
    DNA;
protect(L, DNA) ->
    protect(L - 1, quote(DNA)).

quote(['I' | Rest]) ->
    ['C'] ++ quote(Rest);
quote(['C' | Rest]) ->
    ['F'] ++ quote(Rest);
quote(['F' | Rest]) ->
    ['P'] ++ quote(Rest);
quote(['P' | Rest]) ->
    ['I', 'C'] ++ quote(Rest);
quote(_) ->
    [].

asnat(N) when N == 0 ->
    ['P'];
asnat(N) when N rem 2 == 0 ->
    ['I'] ++ asnat(trunc(N / 2));
asnat(N) ->
    %io:format("~w", [N]),
    ['C'] ++ asnat(trunc(N / 2)).

pattern_loop([], I, E, _, DNA) ->
    {I, E, DNA};
pattern_loop(['C' | T], I, E, C, DNA) ->
    test_b('C', T, I, E, C, DNA);
pattern_loop(['F' | T], I, E, C, DNA) ->   
    test_b('F', T, I, E, C, DNA);
pattern_loop(['P' | T], I, E, C, DNA) ->
    test_b('P', T, I, E, C, DNA);
pattern_loop(['I' | T], I, E, C, DNA) ->
    test_b('I', T, I, E, C, DNA);
pattern_loop([[33, N] | T], I, E, C, DNA) ->
    NewI = I + N,
    if 
	NewI > length(DNA) ->	    
	    {0, E, DNA};
	true ->
	    pattern_loop(T, NewI, E, C, DNA)
    end;
pattern_loop([['?', S] | T], I, E, C, DNA) ->   
   S_Length = length(S),
   case find_S(DNA, S, S_Length, I, I) of
       {true, N} ->
	   pattern_loop(T, N, E, C, DNA);
       false ->
	   {0, E, DNA}
   end;
pattern_loop(['(' | T], I, E, C, DNA) ->
    pattern_loop(T, I, E, [I]++C, DNA);
pattern_loop([')' | T], I, E, [H |Rest], DNA) ->
    if 
	H =< I ->
	    pattern_loop(T, I, E, Rest, DNA);
	true ->
	    pattern_loop(T, I, E++[lists:sublist(DNA, H, I)], Rest, DNA)
    end.   
 
find_S(DNA, S, Len, I, N) ->
    SubList = lists:sublist(DNA, I, N),
    case string:equal(SubList, S) of
	true ->
	    {true, N};
	false ->
	    find_S(DNA, S, Len, I, N+1)
    end.

test_b(Letter, T, I, E, C, DNA) ->
    Nth = lists:nth(I, DNA),
    if 
	Nth == Letter ->	    
	    pattern_loop(T, I+1, E, C, DNA);
	true ->
	    {0, E, DNA}
    end.

template(['C' | T], RNA, Template) ->
    template(T, RNA, Template++['I']);
template(['F' | T], RNA, Template) ->
    template(T, RNA, Template++['C']);
template(['P' | T], RNA, Template) ->
    template(T, RNA, Template++['F']);
template(['I', 'C' | T], RNA, Template) ->
    template(T, RNA, Template++['P']);
template(['I', 'P' | T], RNA, Template) ->
    {DNA, N} = nat(T, RNA),
    {DNA2, L} = nat(DNA, RNA),
    A = [N]++['_']++[L],
    template(DNA2, RNA, Template++[A]);
template(['I', 'F' | T], RNA, Template) ->
    {DNA, N} = nat(T, RNA),
    {DNA2, L} = nat(DNA, RNA),
    A = [N]++['_']++[L],
    template(DNA2, RNA, Template++[A]);
template(['I', 'I', 'P' | T], RNA, Template) ->
    {DNA, N} = nat(T, RNA),
    A = ['|']++[N]++['|'],
    template(DNA, RNA, Template++[A]);
template(['I', 'I', 'C' | T], RNA, Template) ->
    {T, RNA, Template};
template(['I', 'I', 'F' | T], RNA, Template) ->
    {T, RNA, Template};
template(['I', 'I', 'I' | T], RNA, Template) ->
    template(lists:nthtail(10, T), RNA++[lists:sublist(T, 3, 10)], Template);
template(_, RNA, Template) ->
    io:format("Template : ~w~n", [Template]),
    finish(RNA).
