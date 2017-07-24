-module(get).
-export([get/3]).
-export_type([proof/0]).

-type proof() :: [stem:hashes(), ...]. % the last element is the 16-hashes-tuple contained in the root

-spec get(leaf:path(), stem:stem_p(), cfg:cfg()) ->
		 {RootHash::stem:hash(), Value, proof()}
		     when Value :: empty | leaf:leaf().
get(Path, Root, CFG) ->
    S = store:get_stem(Root, CFG),
    H = stem:hash(S, CFG),
    case get2(Path, S, [stem:hashes(S)], CFG) of
	{empty, Proof} -> {H, empty, Proof};
	{A, Proof} -> {H, A, Proof}
    end.       
get2([<<N:4>> | Path], Stem, Proof, CFG) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	0 -> %empty
	    {empty, Proof};
	1 -> %another stem
	    Next = store:get_stem(PN, CFG),
	    get2(Path, Next, [stem:hashes(Next)|Proof], CFG);
	2 -> %leaf
	    Leaf2 = store:get_leaf(PN, CFG),
	    {Leaf2, Proof}
    end.
