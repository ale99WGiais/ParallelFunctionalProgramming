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
    Mappers = [get_mapper(Map,R,Split) || Split <- Splits],
    Mappeds = processDistributed(nodes(), Mappers, 0),
    io:format("Map phase complete\n"),
    Reducers = [get_reducer(Reduce,I,Mappeds) || I <- lists:seq(0,R-1)],
    Reduceds = processDistributed(nodes(), Reducers, 0),
    io:format("Reduce phase complete\n"),
    lists:sort(lists:flatten(Reduceds)).

get_mapper(Map,R,Split) ->
    fun() ->
        Mapped = [{erlang:phash2(K2,R),{K2,V2}} || 
            {K,V} <- Split,
            {K2,V2} <- Map(K,V)
        ],
        group(lists:sort(Mapped))
    end.

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

get_reducer(Reduce,I,Mappeds) ->
    Inputs = [KV || 
        Mapped <- Mappeds,
        {J,KVs} <- Mapped,
        I==J,
        KV <- KVs
    ],
    fun() -> 
        Result = reduce_seq(Reduce,Inputs),
        Result end.


processDistributed(_, [], 0) -> 
    [];
processDistributed(_, [], NbActive) -> 
    Parent = self(),
    {Node, Res} = receive {Parent, Node, Res} -> {Node, Res} end,
    io:format("receive from node ~p \n", [Node]), 
    [Res | processDistributed([], [], NbActive - 1)] ;
processDistributed([], SplitsToProcess, NbActive) ->
    Parent = self(), 
    {Node, Res} = receive {Parent, Node, Res} -> {Node, Res} end,
    io:format("receive from node ~p \n", [Node]), 
    [Res | processDistributed([Node], SplitsToProcess, NbActive - 1)];
processDistributed([AvailableNode | AvailableNodes], [SplitToProcess | SplitsToProcess], NbActive) -> 
    Parent = self(),
    _ = spawn_link(AvailableNode, fun() -> Parent! {Parent, AvailableNode, SplitToProcess()} end),
    io:format("spawned process on node ~p \n", [AvailableNode]), 
    processDistributed(AvailableNodes, SplitsToProcess, NbActive + 1).




%% Idea: write a recursive function that takes: 
%% list of available nodes
%% list of Splits to process
%% number of active processes
%% And returns the list of results. 

%% It can be seen as a state machine:
%% if there are no splits yet to process
%% if there are no active processes: return empty list
%% else: receive a result, return [res | recursiveCall(activeProcesses - 1)]
%% else:
%% if there is a node available: send the split to the worker, return recursiveCall(availableNodes - spawned, splitsToProcess - split, activeProcesses + 1)
%% else: receive a result, return [res | recursiveCall(availableNodes + finished, activeProcesses - 1)


%% The initial set of splitsToProcess is all splits.
%% The initial set of available nodes is nodes().
