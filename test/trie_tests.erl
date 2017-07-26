-module(trie_tests).
-include_lib("eunit/include/eunit.hrl").

-define(ID, trieUnitTest).

api_smoke_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     SupPid
     end,
     fun(SupPid) ->
	     ?assert(is_process_alive(SupPid)),
	     cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Initialization produces empty trie",
	?_test(assert_trie_empty(_Root = 0, ?ID))}
     , {"Put - happy path", fun put_happy_path/0}
     , {"Get - case empty", fun get_empty/0}
     , {"Garbage collection keeping a root (i.e. a stem)", fun gc_keeping_root/0}
     , {"Garbage collection keeping a leaf", fun gc_keeping_leaf/0}
     ]
    }.

key_range_good_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     assert_trie_empty(0, ?ID),
	     SupPid
     end,
     fun(SupPid) ->
	     ?assert(is_process_alive(SupPid)),
	     cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Put key range - case min key",
	?_test(put_and_assert_key(_Key = 0, _Root = 0, ?ID))}
     , {"Put key range - case max key",
	?_test(put_and_assert_key(
		 _Key = (1 bsl (cfg:path(trie:cfg(?ID)) * 8)) - 1,
		 _Root = 0,
		 ?ID))}
     ]
    }.

key_range_bad_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     SupMonRef = erlang:monitor(process, SupPid),
	     unlink(SupPid),
	     assert_trie_empty(0, ?ID),
	     {SupPid, SupMonRef}
     end,
     fun({SupPid, SupMonRef}) ->
	     receive
		 {'DOWN', SupMonRef, process, SupPid, Reason} ->
		     ?debugFmt("~nTrie sup ~p exited for reason ~p~n",
			       [SupPid, Reason]),
		     ok
	     end,
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Put key range - case negative key",
	?_assertException(
	   _, _,
	   trie:put(_Key = -1, _Value = <<1,1>>, _Meta = 0, _Root = 0, ?ID))}
     , {"Put key range - case too big key",
	?_assertException(
	   _, _,
	   trie:put(_Key = 1 bsl (cfg:path(trie:cfg(?ID)) * 8),
		    _Value = <<1,1>>, _Meta = 0, _Root = 0, ?ID))}
     ]
    }.

migrated_from_stem_test_() -> %TODO clarify what this test is for or remove
    {foreach,
        fun() ->
        P = {6,5,4,3,7,8,9,4,5,3,2,6,7,8,3,4},
        T = {0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

        CFG = cfg:new(1, 9, trie, 2, 12),
        {_, _, _, H} = stem:empty(CFG),
        S = {stem, T, P, H},
        S2 = stem:serialize(S, CFG),
        S = stem:deserialize(S2, CFG),
        stem:hash(S, CFG)
        end,
        [
            {"serialize and deserialize stem", ?_assert(true)}
        ]
    }.

cleanup_alive_sup(Sup) when is_pid(Sup) ->
    SupMonRef = erlang:monitor(process, Sup),
    unlink(Sup),
    exit(Sup, Reason = shutdown),
    receive
	{'DOWN', SupMonRef, process, Sup, R} ->
	    Reason = R,
	    ok
    end,
    ok.

assert_trie_empty(Root = 0, Id) ->
    ?assertEqual([], trie:get_all(Root, Id)),
    ok.

put_and_assert_key(Key, Root, Id) ->
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, Id),
    {_, Leaf, _} = trie:get(Key, Root2, Id),
    ?assertEqual(Key, leaf:key(Leaf)),
    ok.

put_happy_path() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    Key = 1,
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, ?ID),
    {Root2Hash, Leaf, Proof} = trie:get(Key, Root2, ?ID),
    ?assertEqual(V, leaf:value(Leaf)),
    ?assert(verify:proof(Root2Hash, Leaf, Proof, trie:cfg(?ID))),
    ?assertEqual([Leaf], trie:get_all(Root2, ?ID)),
    ok.

get_empty() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    ?assertMatch({_, empty, _}, trie:get(_Key = 1, Root, ?ID)),
    ok.

gc_keeping_root() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    Key = 1,
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, ?ID),
    ?assertEqual(ok, trie:garbage([Root2], ?ID)),
    {Root2Hash, Leaf, Proof} = trie:get(Key, Root2, ?ID),
    ?assert(verify:proof(Root2Hash, Leaf, Proof, trie:cfg(?ID))),
    ok.

gc_keeping_leaf() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    Cfg = trie:cfg(?ID),
    K1 = 1,
    V1 = <<1,1>>,
    K2 = 2,
    ?assert(K2 > K1),
    V2 = <<1,2>>,
    Meta = 0,
    Root2 = trie:put(K1, V1, Meta, Root, ?ID),
    Root3 = trie:put(K2, V2, Meta, Root2, ?ID),
    {_, Leaf2, _} = trie:get(K2, Root3, ?ID),
    ?assertEqual(ok, trie:garbage_leaves([{leaf:path(Leaf2, Cfg), Root3}], ?ID)),
    {RootHash, Leaf2, Proof} = trie:get(K2, Root3, ?ID),
    ?assert(verify:proof(RootHash, Leaf2, Proof, Cfg)),
    ok.
