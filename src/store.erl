-module(store).
-export([store/3, store/5, get_branch/5, store_branch/6]).

store(Leaf, Hash, Proof, Root, CFG) -> %this restores information to the merkle trie that had been garbage collected.

    %We should probably calculate the existing branch, and the proof2branch, and mix them together. We want the new branch to contain pointers to the existing data.

    true = verify:proof(Hash, Leaf, Proof, CFG),
    LPointer = leaf:put(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    Path = leaf:path(Leaf, CFG),
    Branch = proof2branch(Proof, 2, LPointer, LH, Path, CFG),
    Branch2 = get_branch(Path, 0, Root, [], CFG),
    Branch3 = combine_branches(Path, Branch, Branch2),
    store_branch(Branch3, Path, 2, LPointer, LH, CFG).
combine_branches(_, X, []) -> X;
combine_branches(<<N:4, Path/bitstring>>, [Sa|A], [Sb|B]) ->%The second one has many pointers we care about. The first one has 1 leaf-pointer we care about.
    [combine_stems(N+1, Sa, Sb)|combine_branches(Path, A, B)].
combine_stems(N, A, B) ->
    T = stem:type(N, A),
    P = stem:pointer(N, A),
    H = element(N, stem:hashes(A)),
    stem:add(B, N-1, T, P, H).
    
proof2branch([],_,_,_, _, _) -> [];
proof2branch([H|T], Type, Pointer, Hash, Path, CFG) -> 
    <<Nibble:4, NewPath/bitstring>> = Path,
    S = stem:recover(Nibble, Type, Pointer, Hash, H),
    NewPointer = stem:put(S, CFG),
    NewHash = stem:hash(S, CFG),
    [S|proof2branch(T, 1, NewPointer, NewHash, NewPath, CFG)].
    
    
store(Leaf, Root, CFG) -> %returns {RootHash, RootPointer, Proof}
    %we could make it faster if the input was like [{Key1, Value1}, {Key2, Value2}...]
    LPointer = leaf:put(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    P = leaf:path(Leaf, CFG),
    B = case get_branch(P, 0, Root, [], CFG) of
	{Leaf2, LP2, Branch} ->%split leaf, add stem(s)
	    %need to add 1 or more stems.
		{A, N2} = path_match(P, leaf:path(Leaf2, CFG), 0),
		[H|T] = empty_stems(A-length(Branch)+1, CFG),
		LH2 = leaf:hash(Leaf2, CFG),
		H2 = stem:add(H, N2, 2, LP2, LH2),
		[H2|T]++Branch;
	    Branch -> %overwrite
		Branch
    end,
    store_branch(B, P, 2, LPointer, LH, CFG).
get_branch(Path, N, Parent, Trail, CFG) ->
    %gather the branch as it currently looks.
    NN = 4*N,
    <<_:NN, A:4, _/bitstring>> = Path,
    M = N+1,
    R = stem:get(Parent, CFG),
    Pointer = stem:pointer(A+1, R),
    RP = [R|Trail],
    case stem:type(A+1, R) of
	0 ->%empty
	    RP;
	1 ->%another stem
	    get_branch(Path, M, Pointer, RP, CFG);
	2 ->%a leaf. 
	    Leaf = leaf:get(Pointer, CFG),
	    case leaf:path(Leaf, CFG) of
		Path -> %overwrite
		    RP;
		_ -> %split leaf, add stem(s)
		    {Leaf, Pointer, RP}
	    end
    end.
store_branch([], Path, _, Pointer, _, CFG) ->
    %Instead of getting the thing, we can build it up while doing store.
    {Hash, _, Proof} = get:get(Path, Pointer, CFG),
    {Hash, Pointer, Proof};

    %case get:get(Path, Pointer, CFG) of
	%{Hash, _, Proof} -> {Hash, Pointer, Proof};
	%empty -> store_branch([], Path, 0, Pointer, 0, CFG)
    %end;
store_branch([B|Branch], Path, Type, Pointer, Hash, CFG) ->
    S = length(Branch),
    NN = 4*S,
    <<_:NN, A:4, _/bitstring>> = Path,
    S1 = stem:add(B, A, Type, Pointer, Hash),
    Loc = stem:put(S1, CFG),
    SH = stem:hash(S1, CFG),
    store_branch(Branch, Path, 1, Loc, SH, CFG).
%add(L) -> add(L, 0).
%add([], X) -> X;
%add([H|T], X) -> add(T, H+X).
path_match(LP, LP2, N) -> %returns {convergense_length, next nibble}
    NN = N*4,
    <<_:NN, A:4, _/bitstring>> = LP,
    <<_:NN, B:4, _/bitstring>> = LP2,
    if
	A == B -> path_match(LP, LP2, N+1);
	true -> {N, B}
    end.
empty_stems(0, _) -> [];
empty_stems(N, CFG) -> [stem:new_empty(CFG)|empty_stems(N-1, CFG)].
