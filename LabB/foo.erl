-module(foo).
-compile([export_all,nowarn_export_all]).

qsort([]) ->
    [];
qsort([X|Xs]) ->
    qsort([Y || Y <- Xs, Y<X]) ++
	[X] ++
	qsort([Y || Y <- Xs, Y>=X]).

asort(L) ->
    asort(L, []).

asort([], Acc) ->
    Acc;
asort([X|Xs],Acc) ->
    asort([Y || Y <- Xs, Y<X],
	  [X | asort([Y || Y <- Xs, Y>=X], Acc)]).

random_list(N) ->
    [rand:uniform(1000000) || _ <- lists:seq(1,N)].

benchmark(Fun,L) ->
    NRuns = 500,
    {T,_} = timer:tc(fun()-> [?MODULE:Fun(L) || _ <- lists:seq(1,NRuns)], ok end),
    T / (1000*NRuns).

bm(Max,Fun,L) ->
    [{I,begin erlang:system_flag(schedulers_online,I),
	      io:format("~p... ",[I]),
	      benchmark(Fun,L)
	end}
      || I <- lists:seq(1,Max)].


psort([]) ->
    [];
psort([X|Xs]) ->
    Parent = self(),
    spawn(fun() ->
		  Parent ! psort([Y || Y <- Xs, Y >= X])
	  end),
    psort([Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive Greater -> Greater end.

psort2(Xs) ->
    psort2(5,Xs).

psort2(0,Xs) ->
    qsort(Xs);
psort2(_,[]) ->
    [];
psort2(D,[X|Xs]) ->
    Parent = self(),
    spawn(fun() ->
		  Parent ! psort2(D-1,[Y || Y <- Xs, Y >= X])
	  end),
    psort2(D-1,[Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive Greater -> Greater end.


psort3(Xs) ->
    psort3(8,Xs).

psort3(0,Xs) ->
    qsort(Xs);
psort3(_,[]) ->
    [];
psort3(D,[X|Xs]) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
		  Parent ! {Ref,psort3(D-1,[Y || Y <- Xs, Y >= X])}
	  end),
    psort3(D-1,[Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive {Ref,Greater} -> Greater end.

psort4(Xs) ->
    psort4(8,Xs).

psort4(0,Xs) ->
    qsort(Xs);
psort4(_,[]) ->
    [];
psort4(D,[X|Xs]) ->
    Parent = self(),
    Ref = make_ref(),
    Less = [Y || Y <- Xs, Y >= X],
    spawn(fun() ->
		  Parent ! {Ref,psort4(D-1,Less)}
	  end),
    psort4(D-1,[Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive {Ref,Greater} -> Greater end.

pasort4(Xs) ->
    pasort4(8,Xs).

pasort4(0,Xs) ->
    asort(Xs);
pasort4(_,[]) ->
    [];
pasort4(D,[X|Xs]) ->
    Parent = self(),
    Ref = make_ref(),
    Less = [Y || Y <- Xs, Y >= X],
    spawn(fun() ->
		  Parent ! {Ref,pasort4(D-1,Less)}
	  end),
    pasort4(D-1,[Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive {Ref,Greater} -> Greater end.

spsort(Xs) ->
  spsort(1,Xs).

spsort(0,Xs) ->
  psort4(5,Xs);
spsort(_,[]) ->
  [];
spsort(D,[X|Xs]) ->
    Parent = self(),
    Ref = make_ref(),
    Less = [Y || Y <- Xs, Y >= X],
    spawn(fun() ->
		  Parent ! {Ref,spsort(D-1,Less)}
	  end),
    spsort(D-1,[Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive {Ref,Greater} -> Greater end.

dsort([]) ->
    [];
dsort([X|Xs]) ->
    Parent = self(),
    Ref = make_ref(),
    Less = [Y || Y <- Xs, Y >= X],
    spawn(baz@JohnsDesktop,
	  fun() ->
		  Parent ! {Ref,psort4(Less)}
	  end),
    psort4([Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive {Ref,Greater} -> Greater end.

less([X|Xs]) ->
    [Y || Y <- Xs, Y >= X].

dsort2([]) ->
    [];
dsort2([X|Xs]) ->
    Parent = self(),
    Ref = make_ref(),
    Less = [Y || Y <- Xs, Y >= X],
    spawn(baz@JohnsTablet2014,
	  fun() ->
		  Parent ! {Ref,psort4(Less)}
	  end),
    psort4([Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive {Ref,Greater} -> Greater end.

dsort3([]) ->
    [];
dsort3([X|Xs]) ->
    Slave = 'mary@CSE-360',
    Parent = self(),
    Ref = make_ref(),
    Less = [Y || Y <- Xs, Y >= X],
    spawn(Slave,
	  fun() ->
		  Parent ! {Ref,psort4(Less)}
	  end),
    psort4([Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive {Ref,Greater} -> Greater end.

wsort(Xs) ->
    wsort(5,Xs).

wsort(0,Xs) ->
    qsort(Xs);
wsort(_,[]) ->
    [];
wsort(D,[X|Xs]) ->
    Less = [Y || Y <- Xs, Y < X],
    case pool:get_worker() of
	no_worker ->
	    wsort(D-1,Less) ++ [X] ++ wsort(D-1,[Y || Y <- Xs, Y >= X]);
	W ->
	    Job = pool:work(W,fun()-> wsort(D-1,Less) end),
	    Greater = wsort(D-1,[Y || Y <- Xs, Y >= X]),
	    pool:done(Job) ++ [X] ++ Greater
    end.

poolsort(Xs) ->
    pool:start(),
    Ys = wsort(Xs),
    pool:stop(),
    Ys.

wsort2(Xs) ->
    wsort2(5,Xs).

wsort2(0,Xs) ->
    qsort(Xs);
wsort2(_,[]) ->
    [];
wsort2(D,[X|Xs]) ->
    Less = [Y || Y <- Xs, Y < X],
    case pool2:get_worker() of
	no_worker ->
	    wsort2(D-1,Less) ++ [X] ++ wsort2(D-1,[Y || Y <- Xs, Y >= X]);
	W ->
	    Job = pool2:work(W,fun()-> wsort2(D-1,Less) end),
	    Greater = wsort2(D-1,[Y || Y <- Xs, Y >= X]),
	    pool2:done(Job) ++ [X] ++ Greater
    end.

poolsort2(Xs) ->
    pool2:start(),
    Ys = wsort2(Xs),
    pool2:stop(),
    Ys.

dwsort(Xs) ->
    Pool = pool(),
    dwsort(Pool,5,Xs).

dwsort2(Xs) ->
    Pool = pool2(),
    dwsort(Pool,5,Xs).

dwsort4(Xs) ->
    Pool = pool4(),
    dwsort(Pool,5,Xs).

dwsort(Pool,0,Xs) ->
    Pool ! {get_node,self()},
    receive 
	{use_node,Node} ->
	    if Node==node() ->
		    Ys = psort4(Xs),
		    Pool ! {available,Node},
		    Ys;
	       true ->
		    Ref = make_ref(),
		    Parent = self(),
		    spawn_link(Node,
			       fun() ->
				       Ys = psort4(Xs),
				       Pool ! {available,Node},
				       Parent ! {Ref,Ys}				       
			       end),
		    receive
			{Ref,Ys} ->
			    Ys
		    end
	    end
    end;
dwsort(_,_,[]) ->
    [];
dwsort(Pool,D,[X|Xs]) when D > 0 ->
    Greater = [Y || Y <- Xs, Y >= X],
    Ref = make_ref(),
    Parent = self(),
    spawn_link(fun() ->
		       Parent ! {Ref,dwsort(Pool,D-1,Greater)}
	       end),
    dwsort(Pool,D-1,[Y || Y <- Xs, Y < X]) ++
	[X] ++
	receive 
	    {Ref,Ys} ->
		Ys
	end.

pool() ->
    Nodes = [node()|nodes()],
    spawn(fun() -> pool(Nodes) end).

pool2() ->
    Nodes = [node()|nodes()],
    spawn(fun() -> pool([N || N <- Nodes,
			      _ <- [1,2]]) end).

pool4() ->
    Nodes = [node()|nodes()],
    spawn(fun() -> pool([N || N <- Nodes,
			      _ <- [1,2,3,4]]) end).
		  

pool([]) ->
    receive
	{available,Node} ->
	    pool([Node])
    end;
pool([Node|Nodes]) ->
    receive 
	{get_node,Pid} ->
	    %%	    io:format("~p ",[Node]),
	    Pid ! {use_node,Node},
	    pool(Nodes)
    end.

schedulers(Node) ->
    rpc:call(Node,erlang,system_info,[schedulers]).

rpc_all(M,F,Args) ->
    [rpc:call(Node,M,F,Args) || Node <- [node()|nodes()]].

load() ->
    rpc_all(code,purge,[foo]),
    {ok,Code} = file:read_file("foo.beam"),
    rpc_all(file,write_file,["foo.beam",Code]),
    rpc_all(code,load_file,[foo]).
