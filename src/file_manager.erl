-module(file_manager).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/3,read/3,write_ram/3,
        terminate2/1]).
init({Name, Size, ram}) ->
    1=2,
    io:fwrite("initiate file manager ram"),
    Z = case db:read(Name) of
	    "" -> hipe_bifs:bytearray(Size, 0);
	    X -> X
	end,
    {ok, {Z, Name, ram}};
init({Name, _, hd}) -> 
    {{ok, F}, _} = {file:open(Name, [write, read, raw, binary]), Name},
    %{ok, F} = file:open(Name, [write, read, raw, binary]),
    {ok, {F, Name}}.
start_link(File, Id, Size, Mode) -> gen_server:start_link({global, Id}, ?MODULE, {File, Size, Mode}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%terminate(_, {Bits, Name, ram}) -> 
%    1=2,
%    db:save(Name, Bits),
%    io:fwrite("ram file died!\n"), 
%    ok;
terminate(_, {F, _}) -> 
    io:fwrite("file_manager died!\n"), 
    file:close(F),
    ok;
terminate(_, X) -> 
    io:fwrite("file manager terminated incorrectly\n"),
    io:fwrite(X).
handle_info(_, X) -> {noreply, X}.
handle_cast(terminate2, X = {F, Name}) -> 
    io:fwrite("cast closing file "),
    file:datasync(F),
    io:fwrite(Name),
    io:fwrite("\n"),
    %file:close(F),
    %timer:sleep(100),
    %{{ok, F2}, _} = {file:open(Name, [write, read, raw, binary]), Name},
    {noreply, {F, Name}};
handle_cast(_, X) -> 
    io:fwrite("unhandled cast in dump file manager\n"),
    {noreply, X}.
handle_call({write, Location, Data}, _From, {Bin, Name, ram}) -> 
    1=2,
    S = size(Data),
    BS = size(Bin),
    if
	Location+S > BS -> Bin;
	true -> 
	    file_manager:write_ram(Location, Data, Bin)
    end,
    {reply, ok, {Bin, Name, ram}};
%handle_call({fast_write, Location, Data}, _From, {X, N}) -> 
%    file:pwrite(X, Location, Data),
%    {reply, ok, {X, N}};
handle_call({write, Location, Data}, _From, {X, N}) -> 
    file:pwrite(X, Location, Data),
    {reply, ok, {X, N}};
handle_call({read, Location, Amount}, _From, {X, Name, ram}) -> 
    1=2,
    A = if
	Location + Amount > size(X) ->
	    eof;
	true ->
		AA = Amount * 8,
		LL = Location * 8,
		<<_:LL, V:AA, _/bitstring>> = X,
		{ok, <<V:AA>>}
    end,
    {reply, A, {X, Name, ram}};
handle_call({read, Location, Amount}, _From, {X, N}) -> 
    {reply, file:pread(X, Location, Amount), {X, N}};
handle_call(_, _From, X) -> {reply, X, X}.
%fast_write(ID, Location, Data) ->
%    I = atom_to_list(ID),
%    A = list_to_atom(I++"_file"),
    %gen_server:call({global, A}, {fast_write, Location, Data}).
%    gen_server:call({global, A}, {write, Location, Data}).
write(ID, Location, Data) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, {write, Location, Data}).
read(ID, Location, Amount) ->
    %io:fwrite("file manager read\n"),
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, {read, Location, Amount}).
terminate2(ID) ->
    io:fwrite("in terminate 2\n"),
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    io:fwrite(A),
    io:fwrite("\n"),
    gen_server:cast({global, A}, terminate2).
%bytes(ID) ->
%    I = atom_to_list(ID),
%    A = list_to_atom(I++"_file"),
%    gen_server:call({global, A}, size).
%dgrow(ID) ->
%    S = bytes(ID),
%    write(ID, S, <<0:80000>>).
write_ram(_, <<>>, Bin) -> Bin;
write_ram(Location, <<D:8, Data/binary>>, Bin) -> 
    1=2,
    hipe_bifs:bytearray_update(Bin, Location, D),
    %spawn(hipe_bifs, bytearray_update, [Bin, Location, D]),
    write_ram(Location+1, Data, Bin).
