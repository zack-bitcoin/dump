-module(utils).
-export([loc2rest/1]).

loc2rest(Loc) ->
    {F, _} = lists:split(length(Loc) - 3, Loc),
    Loc2 = F ++ "_rest.db".
