-module(utils).
-export([save_table/2]).


save_table(ID, Loc) ->
    case ets:tab2file(ID, Loc, [{sync, true}]) of
        ok -> ok;
        {error, R} ->
            save_table(ID, Loc)
    end.
