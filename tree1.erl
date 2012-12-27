% binary search tree
-module(tree1).
-record(node, {l, r, v}). % use rr/1 to load record definitions, see also rd, rf, rl, rp
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

% create a new tree
add(nil, V) ->
    #node{l=nil, r=nil, v=V};
% duplicate key
add(#node{l=_, r=_, v={NodeKey, _}}, {ValueKey, _}) when ValueKey == NodeKey ->
    throw(key_exists);
% add to subtree
add(#node{l=L, r=_, v={NodeKey, _}} = T, {ValueKey, _} = V) when ValueKey < NodeKey ->
    T#node{l=add(L, V)};
add(#node{l=_, r=R, v={NodeKey, _}} = T, {ValueKey, _} = V) when ValueKey > NodeKey ->
    T#node{r=add(R, V)}.

traverse_in_order(nil, _, VisitorData) -> VisitorData;
traverse_in_order(#node{l=nil, r=nil, v=V}, Visitor, VisitorData) ->
    Visitor(VisitorData, V);
traverse_in_order(#node{l=L, r=R, v=_} = T, Visitor, VisitorData) ->
    A = traverse_in_order(L, Visitor, VisitorData),
    B = traverse_in_order(T#node{l=nil, r=nil}, Visitor, A),
    traverse_in_order(R, Visitor, B).

print(T) ->
    print(T, "").
print(nil, _) -> ok;
print(#node{l=nil, r=nil, v={NodeKey, NodeValue}}, Prefix) ->
    io:format("~s- ~w = ~w~n", [Prefix, NodeKey, NodeValue]);
print(#node{l=L, r=R} = T, Prefix) ->
    print(T#node{l=nil, r=nil}, Prefix),
    print(L, Prefix ++ " L "),
    print(R, Prefix ++ " R ").

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

create_random_list(Elements) ->
    lists:sort(fun(_, _) -> random:uniform() > 0.5 end, lists:seq(1, Elements)).
create_tree_from_list(Vals) ->
    lists:foldl(fun(Val, Tree) -> add(Tree, {Val, Val}) end, nil, Vals).
create_test() ->
    #node{l=nil, r=nil, v={1, 2}} = add(nil, {1, 2}).
ordered_traversal_test() ->
    % added in order
    ordered_traversal_test(lists:seq(1, 50), lists:seq(1, 50)),
    % added backwards
    ordered_traversal_test(lists:reverse(lists:seq(1, 50)), lists:seq(1, 50)),
    % added in random order
    ordered_traversal_test(create_random_list(50), lists:seq(1, 50)).
ordered_traversal_test(Vals, ExpectedVals) ->
    Tree = create_tree_from_list(Vals),
    ExpectedVals = traverse_in_order(Tree, fun(L, {K, _}) -> L ++ [K] end, []).
duplicate_key_add_test() ->
    Tree = create_tree_from_list(create_random_list(50)),
    try add(Tree, {22, 22})
    catch key_exists -> ok
    end.

-endif.
