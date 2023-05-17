-module(foo).

-compile(export_all).

foo() -> io:format(user,"hello",[]).

myred(K, []) -> 0;
myred(K, [V|Vs]) -> V + myred(K, Vs).

mymap(K, V) -> V * 2.
