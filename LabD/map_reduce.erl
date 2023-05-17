%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a very simple implementation of map-reduce, in both 
%% sequential and parallel versions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(map_reduce).
-compile([export_all,nowarn_export_all]).

%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce. 

%% The input is a collection of key-value pairs. The map function maps
%% each key value pair to a list of key-value pairs. The reduce
%% function is then applied to each key and list of corresponding
%% values, and generates in turn a list of key-value pairs. These are
%% the result.

map_reduce_seq(Map,Reduce,Input) ->
    Mapped = [{K2,V2}
	      || {K,V} <- Input,
		 {K2,V2} <- Map(K,V)],
    io:format("Map phase complete\n"),
    reduce_seq(Reduce,Mapped).

reduce_seq(Reduce,KVs) ->
    [KV || {K,Vs} <- group(lists:sort(KVs)),
	   KV <- Reduce(K,Vs)].

group([]) ->
    [];
group([{K,V}|Rest]) ->
    group(K,[V],Rest).

group(K,Vs,[{K,V}|Rest]) ->
    group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
    [{K,lists:reverse(Vs)}|group(Rest)].

map_reduce_par(Map,M,Reduce,R,Input) ->
    Parent = self(),
    Splits = split_into(M,Input),
    NodesMap = generateListOfNodes (M),
    Mappers = 
	[spawn_mapper(Parent,Map,R,Split,Node)
	 || {Split, Node} <- lists:zip(Splits, NodesMap)],
    Mappeds = 
	[receive {Pid,L} -> L end || Pid <- Mappers],
    io:format("Map phase complete\n"),
    NodesRed = generateListOfNodes (R),
    Reducers = 
	[spawn_reducer(Parent,Reduce,I,Mappeds, Node) 
	 || {I, Node} <- lists:zip(lists:seq(0,R-1), NodesRed)],
    Reduceds = 
	[receive {Pid,L} -> L end || Pid <- Reducers],
    io:format("Reduce phase complete\n"),
    lists:sort(lists:flatten(Reduceds)).

spawn_mapper(Parent, Map,R,Split, Node) ->
    io:format("spawn Mapper on node ~s \n", [Node]),
    spawn_link(Node, fun() ->
			Mapped = [{erlang:phash2(K2,R),{K2,V2}}
				  || {K,V} <- Split,
				     {K2,V2} <- Map(K,V)],
                        io:format("."),
			Parent ! {self(),group(lists:sort(Mapped))}
		end).

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds, Node) ->
    io:format("spawn Reducer on node ~s \n", [Node]),
    Inputs = [KV
	      || Mapped <- Mappeds,
		 {J,KVs} <- Mapped,
		 I==J,
		 KV <- KVs],
    spawn_link(Node, fun() -> Result = reduce_seq(Reduce,Inputs),
                        io:format("."),
                        Parent ! {self(),Result} end).




cycle ([X|Xs]) -> {X, append_last(X, Xs)} .

append_last (X, List) ->
    ListReversed = lists:reverse(List),
    NewList = [X | ListReversed],
    Res = lists:reverse(NewList),
    Res.



generateListOfNodes (HowMany) -> 
    List = nodes(),
    Len = length(List),
    Result = lists:map(fun(Num) -> lists:nth((Num rem Len) + 1, List) end, lists:seq(0,HowMany-1)),
    Result.
