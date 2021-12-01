-module(bits).
-behaviour(gen_server).
-export([start_link/3,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
         write/1,top/1,highest/1,delete/2,get/2]).
init({ID, Loc}) ->
    process_flag(trap_exit, true),
    case ets:info(ID) of
        undefined ->
            case ets:file2tab(Loc) of
                {ok, ID} -> ok;
                {error, _R} ->
                    ets:new(ID, [set, named_table, {write_concurrency, false}, compressed])
            end;
        _ -> ok
    end,
    {Top, %lowest empty location
     Highest} = %highest filled location
    case db:read(utils:loc2rest(Loc)) of
        "" -> {1, 1};
        X -> 
            {Y, Z} = binary_to_term(X),
            {Y, Z}
    end,
    {ok, {ID, Top, Highest, Loc}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
start_link(ID, File, _Size) -> gen_server:start_link({global, ID}, ?MODULE, {ID, File}, []).
terminate(_, {ID, Top, Highest, Loc}) -> 
    Loc2 = utils:loc2rest(Loc),
    db:save(Loc2, term_to_binary({Top, Highest})),
%    db:save(File, Bits),
    utils:save_table(ID, Loc),
    io:format("bits died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({delete, Height}, _From, {ID, Top, Highest, File})-> 
    ets:delete(ID, Height),
    NewHighest = 
        case Height of 
            Highest -> highest2(ID, Highest);
            true -> Highest
        end,
    {reply, ok, {ID, min(Top,Height), 
                 NewHighest, File}};
handle_call(write, _From, {ID, Top, H, File}) -> 
    ets:insert(ID, {Top}),
    NewTop = top2(ID, Top+1),
    {reply, Top, {ID, NewTop, max(H, Top), File}};
handle_call({get, N}, _From, {ID, Top, H, File}) -> 
    G = case ets:lookup(ID, N) of
            [] -> 0;
            _ -> 1
        end,
    {reply, G, {ID, Top, H, File}};
handle_call(top, _From, {ID, Top, H, File}) -> 
    {reply, Top, {ID, Top, H, File}};
handle_call(highest, _From, {ID, Top, Highest, File}) -> 
    {reply, Highest, {ID, Top, Highest, File}};
handle_call(_, _From, X) -> {reply, X, X}.
ider(ID) -> list_to_atom(atom_to_list(ID)++"_bits").
get(ID, N) -> gen_server:call({global, ider(ID)}, {get, N}).
delete(ID, Height) -> 
    gen_server:call({global, ider(ID)}, {delete, Height}).
write(ID) -> 
    gen_server:call({global, ider(ID)}, write).
    
highest(ID) -> gen_server:call({global, ider(ID)}, highest).
top(ID) -> gen_server:call({global, ider(ID)}, top).

highest2(_ID, 1) -> 1;
highest2(ID, N) ->
    case ets:lookup(ID, N) of
        [] -> highest2(ID, N-1);
        _ -> N
    end.
top2(ID, N) ->
    case ets:lookup(ID, N) of
        [] -> N;
        _ -> top2(ID, N+1)
    end.
            
