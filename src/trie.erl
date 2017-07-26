-module(trie).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, root_hash/2,cfg/1,get/3,put/5,delete/3,garbage/2,garbage_leaves/2,get_all/2]).
-record(state,{
    cfg
}).

init(Args) ->
    CFG = Args,
    0 = store:put_stem(stem:new_empty(CFG), CFG),
    {ok, #state{cfg = CFG}}.
start_link(Args) -> %keylength, or M is the size outputed by hash:doit(_).
    CFG = Args,
    gen_server:start_link({local, ids:main(CFG)}, ?MODULE, Args, []).

code_change(_OldVsn, State, _Extra) ->
    %TODO log here
    {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) ->
    %TODO log here
    {noreply, X}.

handle_cast({garbage, Keepers}, State = #state{cfg = CFG}) ->
    garbage:garbage(Keepers, CFG),
    {noreply, State};
handle_cast({garbage_leaves, KLS}, State = #state{cfg = CFG}) ->
    garbage:garbage_leaves(KLS, CFG),
    {noreply, State};
handle_cast(_, X) ->
    %TODO log here
    {noreply, X}.

handle_call({delete, Key, Root}, _From, State = #state{cfg = CFG}) ->
    NewRoot = delete:delete(Key, Root, CFG),
    {reply, NewRoot, State};
handle_call({put, Key, Value, Meta, Root}, _From, State = #state{cfg = CFG}) ->
    Leaf = leaf:new(Key, Value, Meta, CFG),
    {_, NewRoot, _} = store:store(Leaf, Root, CFG),
    {reply, NewRoot, State};
handle_call({get, Key, RootPointer}, _From, State = #state{cfg = CFG}) ->
    P = leaf:path_maker(Key, CFG),
    {RootHash, L, Proof} = get:get(P, RootPointer, CFG),
    L2 = if
	     L == empty -> empty;
	     true ->
		 Key2 = leaf:key(L),
		 if
		     Key == Key2 -> L;
		     true -> empty
		 end
	 end,
    {reply, {RootHash, L2, Proof}, State};
handle_call({get_all, Root}, _From, State = #state{cfg = CFG}) ->
    X = get_all_internal(Root, CFG),
    {reply, X, State};
handle_call({garbage_leaves, KLS}, _From, State = #state{cfg = CFG}) ->
    garbage:garbage_leaves(KLS, CFG),
    {reply, ok, State};
handle_call({root_hash, RootPointer}, _From, State = #state{cfg = CFG}) ->
    S = store:get_stem(RootPointer, CFG),
    H = stem:hash(S, CFG),
    {reply, H, State};
handle_call(cfg, _From, State = #state{cfg = CFG}) ->
    {reply, CFG, State};

handle_call(_, _From, State) ->
    %TODO log here
    {reply, ok, State}.

cfg(ID) when is_atom(ID) ->
    gen_server:call(ids:main_id(ID), cfg).
root_hash(ID, RootPointer) when is_atom(ID) ->
    gen_server:call(ids:main_id(ID), {root_hash, RootPointer}).
-spec put(leaf:key(), leaf:value(), leaf:meta(), stem:stem_p(), atom()) ->
		 stem:stem_p().
put(Key, Value, Meta, Root, ID) ->
    gen_server:call(ids:main_id(ID), {put, Key, Value, Meta, Root}).
-spec get(leaf:key(), stem:stem_p(), atom()) ->
		 {stem:hash(), empty | leaf:leaf(), get:proof()}.
get(Key, Root, ID) -> gen_server:call(ids:main_id(ID), {get, Key, Root}).
get_all(Root, ID) -> gen_server:call(ids:main_id(ID), {get_all, Root}).
-spec delete(leaf:key(), stem:stem_p(), atom()) -> stem:stem_p().
delete(Key, Root, ID) -> gen_server:call(ids:main_id(ID), {delete, Key, Root}).
-spec garbage([stem:stem_p()], atom()) -> ok.
garbage(Keepers, ID) -> 
    %io:fwrite("trie garbage \n"),
    gen_server:cast(ids:main_id(ID), {garbage, Keepers}).
garbage_leaves(KLS, ID) ->
    gen_server:cast(ids:main_id(ID), {garbage_leaves, KLS}).


get_all_internal(Root, CFG) ->
    S = store:get_stem(Root, CFG),
    P = tuple_to_list(stem:pointers(S)),
    T = tuple_to_list(stem:types(S)),
    get_all_internal2(P, T, CFG).
get_all_internal2([], [], _) -> [];
get_all_internal2([A|AT], [T|TT], CFG) -> 
    B = case T of
	    0 -> %empty
		[];
	    1 -> %another stem
		get_all_internal(A, CFG);
	    2 -> %a leaf.
		[store:get_leaf(A, CFG)]
	end,
    B++get_all_internal2(AT, TT, CFG).
