-module(dump).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,put/2,get/2,word/1,update/3, put_batch/2, mode/1,
         delete_all/1,%only in ram mode,
         reload/1,
         quick_save/1
        ]).
init({Mode, WordSize, ID, Loc}) -> 
    process_flag(trap_exit, true),
    Top = case Mode of
            ram -> 
                load_ets(ID, Loc);
            hd -> bits:top(ID)
        end,
    %io:fwrite("start dump0\n"),
    %io:fwrite("top is "),
    %io:fwrite(integer_to_list(Top)),
    %io:fwrite("\n"),
    %io:fwrite(integer_to_list(W)),
    %io:fwrite("start dump1\n"),
    %io:fwrite("\n"),
    if
        not(is_integer(WordSize)) -> 
            io:fwrite({WordSize, Mode});
        true -> ok
    end,
    true = is_integer(WordSize),
    {ok, {Mode, Top, WordSize, ID, Loc}}.
start_link(WordSize, Id, Mode, Loc) -> 
    X = case Mode of
             ram -> {ram, WordSize, Id, Loc};
             hd -> 
                true = is_integer(WordSize),
                {hd, WordSize, Id, Loc}
         end,
    gen_server:start_link({global, Id}, ?MODULE, X, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
loc2rest(Loc) ->
    {F, _} = lists:split(length(Loc) - 3, Loc),
    F ++ "_rest.db".
terminate(_, {ram, Top, _WordSize, ID, Loc}) -> 
    Loc2 = loc2rest(Loc),
    db:save(Loc2, term_to_binary({Top})),
    utils:save_table(ID, Loc),
    io:format("dump died!\n"), 
    ok;
terminate(_, _) -> 
    io:format("dump died!\n"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(delete_all, {ram, _, W, ID, Loc}) -> 
    ets:delete_all_objects(ID),
    %{noreply, {ram, Top, W, ID, Loc}};
    {noreply, {ram, 1, W, ID, Loc}};
handle_cast(reload, {hd, _, WordSize, ID, Loc}) -> 
    bits:load_ets_external(ID),
    Top2 = read_top(Loc),
    true = is_integer(Top2),
    {noreply, {hd, Top2, WordSize, ID, Loc}};
handle_cast(reload, {ram, _Top, WordSize, ID, Loc}) -> 
    ets:delete(ID),
    timer:sleep(100),
    load_ets(ID, Loc),
    Top2 = read_top(Loc),
    true = is_integer(Top2),
    {noreply, {ram, Top2, WordSize, ID, Loc}};
handle_cast(_, X) -> {noreply, X}.
handle_call(quick_save, _, X = {Type, Top, _WordSize, ID, Loc}) -> 
    Loc2 = loc2rest(Loc),
    db:save(Loc2, term_to_binary({Top})),
    case Type of
        ram ->
            utils:save_table(ID, Loc);
        hd -> bits:quick_save(ID)
    end,
    {reply, ok, X};
handle_call(mode, _From, X = {Y, _, _, _, _}) ->
    {reply, Y, X};
handle_call({delete, Location, _Id}, _From, X = {hd, _, _, Id, _}) ->
    bits:delete(Id, Location),
    {reply, ok, X};
handle_call({delete, Location, _}, _From, X = {ram, _, _, Id, _}) ->
    ets:delete(Id, Location),
    {reply, ok, X};
handle_call({update, Location, Data, _ID}, _From, X = {ram, _, _, ID, _}) ->
    ets:insert(ID, [{Location, Data}]),
    {reply, ok, X};
handle_call({update, Location, Data, _ID}, _From, X = {hd, _, _, ID, _}) ->
    Word = size(Data),
    file_manager:write(ID, Location*Word, Data),
    {reply, ok, X};
handle_call({write, Data, _ID}, _From, {ram, Top, WordSize, ID, Loc}) ->
    ets:insert(ID, {Top, Data}),
    {reply, Top, {ram, Top+1, WordSize, ID, Loc}};
handle_call({write, Data, _ID}, _From, {hd, _Top, Word, ID, Loc}) ->
    Word = size(Data),
    Top = bits:top(ID),
    file_manager:write(ID, Top*Word, Data),
    bits:write(ID),
    Top2 = bits:top(ID),
    {reply, Top, {hd, Top2, Word, ID, Loc}};
handle_call({read, Location, _ID}, _From, X = {ram, _, _, ID, _}) ->
    Y = case ets:lookup(ID, Location) of
            [] -> empty;
            Z -> element(2, hd(Z))
        end,
    {reply, Y, X};
handle_call({read, Location, _ID}, _From, X = {hd, _, Word, ID, _Loc}) ->
    Z = case file_manager:read(ID, Location*Word, Word) of
	    {ok, A} -> A;
	    eof -> 
		<<0:(Word*8)>>
	end,
    {reply, Z, X};
handle_call(word, _From, X = {ram, _, _, _, _}) ->
    {reply, 0, X};
handle_call(word, _From, X = {hd, _, Word, _, _}) ->
    {reply, Word, X};
%handle_call({highest, _ID}, _From, X = {ram, Top, _, _}) ->
%    {reply, Top, X};
%handle_call({highest, ID}, _From, X = {hd, Word, _, _}) ->
%    A = bits:highest(ID),
%    {reply, A*Word, X};
handle_call(Other, _, X) ->
    io:fwrite("dump cannot handle that command\n"),
    io:fwrite("\n"),
    %io:fwrite({Other, X}),
    {reply, ok, X}.


load_ets(ID, Loc) ->
    case ets:info(ID) of
        undefined ->
            case ets:file2tab(Loc) of
                {ok, ID} -> ok;
                {error, _R} ->
                    ets:new(ID, [set, named_table, {write_concurrency, false}, compressed])
            end,
            1;
        _ -> read_top(Loc)
    end.
read_top(Loc) ->
    case db:read(loc2rest(Loc)) of
        "" -> 1;
        X -> 
            {Y} = binary_to_term(X),
            Y
    end.


quick_save(ID) ->
    gen_server:call({global, ID}, quick_save).
reload(ID) ->
    gen_server:cast({global, ID}, reload).
delete_all(ID) ->
    gen_server:cast({global, ID}, delete_all).
    
delete(X, ID) -> gen_server:call({global, ID}, {delete, X, ID}).
update(Location, Data, ID) -> 
    gen_server:call({global, ID}, {update, Location, Data, ID}).
put(Data, ID) -> 
    gen_server:call({global, ID}, {write, Data, ID}).
put_batch(L, ID) -> 
    gen_server:call({global, ID}, {write_batch, L, ID}).
get(X, ID) -> 
    true = X > 0,
    gen_server:call({global, ID}, {read, X, ID}).
word(ID) -> gen_server:call({global, ID}, word).
%highest(ID) -> gen_server:call({global, ID}, {highest, ID}).
mode(ID) -> gen_server:call({global, ID}, mode).
