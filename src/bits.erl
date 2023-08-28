-module(bits).
-behaviour(gen_server).
%A database that stores a list of bits.
%only used in hd mode.

-export([start_link/3,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,

write/1,%sets the lowest zero-bit to one.
delete/2,%sets a one-bit to zero.
top/1,%the lowest zero-bit
%highest/1,%the highest one-bit
get/2,%checks if a bit is zero or one.
load_ets_external/1,%loads from the database (used after downloading the checkpoint.)
quick_save/1,%saves the current version to the database.
reset/1,%deletes everything.

ider/1,

test/0
]).

-define(page, 1024).%bytes per page
-define(page_bits, (?page * 8)).
    

init({ID, File, _Size}) ->
    process_flag(trap_exit, true),
	%case db:read(File) of
    load_ets(ID, File),
    Top = top2(ID, 1),
    %Highest = highest2(ID, Size-1),
    {ok, {ID, Top, ok, File, ok}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
start_link(Id, File, Size) -> gen_server:start_link({global, Id}, ?MODULE, {Id, File, Size}, []).
terminate(_, {ID, Top, _Highest, File, _Size}) -> 
    %db:save(File, {Bits, Top, Highest}),
    %ets:insert(ID, [{top, Top}]),
    utils:save_table(ID, File),
    io:format("bits died!\n"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(load_ets, X = {ID, _Top, _Highest, File, Size}) ->
    io:fwrite("bits internal load ets 1\n"),
    ets:delete(ID),
    io:fwrite("bits internal load ets 2\n"),
    case ets:file2tab(File) of
        {ok, ID} ->
            %{ok, ID} = ets:file2tab(File),
            io:fwrite("bits internal load ets 3\n"),
            Top = ets_top_check(ID),
            io:fwrite("bits internal load ets 4\n"),
            {noreply, {ID, Top, ok, File, Size}};
        _ ->
            io:fwrite("bits failed to load a new ets\n"),
            {noreply, X}
    end;
handle_cast(quick_save, X = {ID, Top, _Highest, File, Size}) -> 
    io:fwrite("quick saving \n"),
    ets:insert(ID, [{top, Top}]),
    utils:save_table(ID, File),
    io:fwrite("quick saved \n"),
    {noreply, X};
handle_cast(reset, X = {ID, Top, _Highest, File, Size}) -> 
    ets:delete_all_objects(ID),
    {noreply, {ID, 1, ok, File, Size}};
handle_cast(_, X) -> {noreply, X}.
handle_call({delete, Height}, _From, {Bits, Top, _Highest, File, Size})-> 
    NewBits = internal_update(Bits, Height, false),
    %%NewBits = hipe_bifs:bitarray_update(Bits, Height, false),%todo
    %NewHighest = highest2(NewBits, Highest),
    {reply, ok, {NewBits, min(Top,Height), ok, File, Size}};
handle_call(write, _From, {Bits, Top, H, File, Size}) -> 
    %NewBits = hipe_bifs:bitarray_update(Bits, Top, true),%todo
    NewBits = internal_update(Bits, Top, true),
    NewTop = top2(NewBits, Top+1),
    {reply, ok, {NewBits, NewTop, max(H, Top), File, Size}};
handle_call({get, N}, _From, X = {Bits, Top, _H, File, Size}) -> 
    G = internal_get(Bits, N),
    {reply, G, X};
handle_call(top, _From, {Bits, Top, H, File, Size}) -> 
    {reply, Top, {Bits, Top, H, File, Size}};
%handle_call(highest, _From, {Bits, Top, Highest, File, Size}) -> 
%    {reply, Highest, {Bits, Top, Highest, File, Size}};
handle_call(_, _From, X) -> {reply, X, X}.
ider(ID) -> list_to_atom(atom_to_list(ID)++"_bits").
get(ID, N) -> gen_server:call({global, ider(ID)}, {get, N}).
delete(ID, Height) -> 
    gen_server:call({global, ider(ID)}, {delete, Height}).
write(ID) -> 
    gen_server:call({global, ider(ID)}, write).
    
%highest(ID) -> gen_server:call({global, ider(ID)}, highest).
top(ID) -> gen_server:call({global, ider(ID)}, top).
top2(Bits, N) ->
    B = internal_get(Bits, N),
    if
	B -> top2(Bits, N+1);
	true -> N
    end.
load_ets_external(ID) ->
    gen_server:cast({global, ider(ID)}, load_ets).
quick_save(ID) ->
    gen_server:cast({global, ider(ID)}, quick_save).
reset(ID) ->
    gen_server:cast({global, ider(ID)}, reset).
    

blank_page() ->
    <<0:(?page_bits)>>.
internal_get(ID, N) ->
    PageNumber = N div ?page_bits,
    PageN = N rem ?page_bits,%Nth bit on the page
    Page = case ets:lookup(ID, PageNumber) of
               [] -> blank_page();
               [{PageNumber, X}] -> X
           end,
    %?page_bits - 1 - (?page_bits - N)
    %N - 1
    PageN_1 = PageN,
    PageBits_PageN = ?page_bits - PageN - 1,
    %io:fwrite("bits internal get "),
    %io:fwrite(integer_to_list(PageN_1)),
    %io:fwrite(" "),
    %io:fwrite(integer_to_list(PageBits_PageN)),
    %io:fwrite("\n"),
    <<_:PageN_1, 
      Bit:1, 
      _:PageBits_PageN>> = Page,
    Bit == 1.
    %hipe_bifs:bitarray_sub(Bits, N).%todo
internal_update(ID, N, Value) ->
    case Value of
        true -> ok;
        false -> ok
    end,
    PageNumber = N div ?page_bits,
    PageN = N rem ?page_bits,
    Page = case ets:lookup(ID, PageNumber) of
               [] -> blank_page();
               [{PageNumber, X2}] -> X2
           end,
    Y = <<0:(PageN),
          1:1,
          0:(?page_bits - PageN - 1)>>,
    <<X:(?page_bits)>> = Y,
    <<PageX:(?page_bits)>> = Page,
    Page2X = case Value of
                true -> 
                    PageX bor X;
                false ->
                    PageX band (bnot(X))
            end,
    %io:fwrite("page2x is "),
    %io:fwrite(integer_to_list(Page2X)),
    %io:fwrite("\n"),
    Page2 = <<Page2X:?page_bits>>,
    ets:insert(ID, [{PageNumber, Page2}]),
    ID.
    
          
    %ets:insert(ID, [{Location, Data}]).
    %hipe_bifs:bitarray_update(Bits, Height, Value).%todo

ets_top_check(ID) ->
    io:fwrite("ets top check\n"),
    case ets:lookup(ID, top) of
        [] -> 1;
        [{top, X}] -> X
    end.
            


load_ets(ID, File) ->
    case ets:info(ID) of
        undefined ->
            case ets:file2tab(File) of
                {ok, ID} -> ets_top_check(ID);
                {error, _} ->
                    ets:new(ID, [set, named_table, {write_concurrency, false}, compressed]),
                    1
            end;
        _ -> ets_top_check(ID)
    end.
    

        
%highest2(_, 1) -> 1;
%highest2(Bits, N) -> 
%    case internal_get(Bits, N) of
%	true -> N;	    
%	false -> highest2(Bits, N-1)
%    end.

test() ->
    reset(dump),
    1 = top(dump),
    write(dump),
    2 = top(dump),
    write(dump),
    3 = top(dump),
    delete(dump, 1),
    1 = top(dump),
    write(dump),
    3 = top(dump),
    write(dump),
    write(dump),
    write(dump),
    write(dump),
    write(dump),
    write(dump),
    9 = top(dump),
    delete(dump, 3),
    3 = top(dump),
    write(dump),
    9 = top(dump),
    success.
    
    
