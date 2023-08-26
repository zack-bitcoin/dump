-module(file_manager).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/3,read/3,reload/1,sync/1]).
init({Name, _, hd}) -> 
    process_flag(trap_exit, true),
    {{ok, F}, _} = {file:open(Name, [write, read, raw, binary]), Name},
    %{ok, F} = file:open(Name, [write, read, raw, binary]),
    {ok, {F, Name}}.
start_link(File, Id, Size, Mode) -> gen_server:start_link({global, Id}, ?MODULE, {File, Size, Mode}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, {F, _}) -> 
    io:fwrite("file_manager died!\n"), 
    file:close(F),
    ok;
terminate(_, X) -> 
    io:fwrite("file manager terminated incorrectly\n"),
    io:fwrite(X).
handle_info(_, X) -> {noreply, X}.
handle_cast(reload, {_, Name}) -> 
    {ok, {F, Name}} = init({Name, ok, hd}),
    {noreply, {F, Name}};
handle_cast(sync, {F, Name}) -> 
    ok = file:sync(F),
    {noreply, {F, Name}};
handle_cast(_, X) -> 
    io:fwrite("unhandled cast in dump file manager\n"),
    {noreply, X}.
handle_call({write, Location, Data}, _From, {X, N}) -> 
    %io:fwrite("file manager write\n"),
    file:pwrite(X, Location, Data),
    {reply, ok, {X, N}};
handle_call({read, Location, Amount}, _From, {X, N}) -> 
    %io:fwrite("file manager read\n"),
    {reply, file:pread(X, Location, Amount), {X, N}};
handle_call(_, _From, X) -> 
    io:fwrite("unhandled call in dump file manager\n"),
    {reply, X, X}.
write(ID, Location, Data) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, {write, Location, Data}).
read(ID, Location, Amount) ->
    %io:fwrite("file manager read\n"),
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, {read, Location, Amount}).
reload(ID) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:cast({global, A}, reload).
sync(ID) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:cast({global, A}, sync).
