-module(utils).
-export([loc2rest/1, save_table/2]).

loc2rest(Loc) ->
    {F, _} = lists:split(length(Loc) - 3, Loc),
    Loc2 = F ++ "_rest.db".

save_table(ID, Loc) ->
    case ets:tab2file(ID, Loc, [{sync, true}]) of
        ok -> ok;
        {error, R} ->
            save_table(ID, Loc)
    end.
    
