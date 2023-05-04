-module(test_dump).
-export([test/0, test2/0, test3/0, test4/0, test_main/2, testlta/0,
        test_batch/0, test_restore_1/0, test_restore_2/0]).
%timer:tc(test_dump, test, []).
%{5457078,success}

-define(test_mode, hd).
%-define(test_mode, ram).

test_batch() ->
    ID = table,
    dump_sup:start_link(ID, 0, 100002, ram, ""),
    L = [{1, 1}, {2, 2}, {3, 3}, {4, 4}],
    dump:put_batch(L, ID),
    dump:get(3, ID).

test_init(ID, Size) ->
    %ets:delete_all_objects(bits:ider(ID)),
    Mode = ?test_mode,
    dump_sup:start_link(ID, Size, 100002, Mode, ""),
    case Mode of
        hd ->
            bits:reset(ID),
            1 = bits:top(ID);
        ram -> dump:delete_all(ID)
    end,
    ok.
    

testlta() ->
    %41077 = 0.04 seconds
    X = abcdefghijk,
    testlta2(100000, X).
testlta2(0, _) -> ok;
testlta2(N, X) -> 
    _ = list_to_atom(atom_to_list(X)),
    testlta2(N-1, X).
test() ->
    ID = kv2,
    Size = 100,
    test_init(ID, Size),
    %timer:tc(test_dump, test_main, [ID, Size]).
    test_dump:test_main(ID, Size).
test_main(ID, Size) ->
    V1 = <<1:(8*Size)>>,
    V2 = <<2:(8*Size)>>,
    V3 = <<3:(8*Size)>>,
    A1 = 1,
    A2 = 2,
    A3 = 3,
    A1 = dump:put(V1, ID),
    V1 = dump:get(A1, ID),
    A2 = dump:put(V2, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    A3 = dump:put(V3, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    V3 = dump:get(A3, ID),
    dump:delete(A1, ID),
    %A1 = dump:put(V2, ID),
    A4 = dump:put(V2, ID),
    Times = 1000,
    Locs = put_times(Times, Size, ID),
    get_times(Locs, Size, ID),
    %V2 = dump:get(A1, ID),
    %dump:update(A1, V1, ID),
    %V1 = dump:get(A1, ID).
    V2 = dump:get(A4, ID),
    dump:update(A4, V1, ID),
    V1 = dump:get(A4, ID),
    success.
    
   
put_times(0, _, _) -> [];
put_times(N, Size, ID) -> 
    [dump:put(<<1:(8*Size)>>, ID)|
     put_times(N-1, Size, ID)].
get_times([], _, _) -> success;
get_times([H|T], Size, ID) ->
    SS = 8*Size, 
    <<1:SS>> = dump:get(H, ID),
    get_times(T, Size, ID).

test2() ->
    ID = test2,
    Size = 100,
    test_init(ID, Size),
    cprof:start(),
    test_dump:test_main(ID, Size),
    cprof:pause(),
    X = cprof:analyse(),
    cprof:stop(),
    X.

test3() ->
    ID = test3,
    Size = 100,
    test_init(ID, Size),
    fprof:apply(test_dump, test_main, [ID, Size]),
    fprof:profile(),
    fprof:analyse().

test4() ->
    ID = test4,
    Size = 100,
    {ok, _X} = test_init(ID, Size),
    eprof:start(),
    %eprof:start_profiling([self()]),
    eprof:start_profiling([dump_ids:bits(ID), ID, dump_ids:file_manager(ID)]),
    test_main(ID, Size),
    eprof:stop_profiling(),
    eprof:analyze(total).

test_restore_clean() ->
    ID = kv2,
    dump:delete_all(ID),
    os:cmd("rm data/*.db").
    

test_restore_1() ->
    ID = kv2,
    Size = 100,
    Mode = dump_app:mode(),
    test_restore_clean(),
    test_init(ID, Size),
    1 = case Mode of
            hd -> dump:top(ID);
            ram -> dump:top(ID)
        end,
    V1 = <<1:(8*Size)>>,
    V2 = <<2:(8*Size)>>,
    S2 = (Size*8),
    case dump:get(2, ID) of
        empty -> ok;
        <<0:S2>> -> ok;
        X -> io:fwrite({size(X), X})
    end,
    dump:put(V1, ID),
    2 = dump:put(V2, ID),
    dump:quick_save(ID).

test_restore_2() ->
    ID = kv2,
    Size = 100,
    Mode = dump_app:mode(),
    %V1 = crypto:strong_rand_bytes(Size)
    V1 = <<1:(8*Size)>>,
    V2 = <<2:(8*Size)>>,
    test_init(ID, Size),
    case Mode of
        hd ->
            1 = dump:top(ID);
        ram -> 1 = dump:top(ID)
    end,
    S2 = (Size*8),
    io:fwrite("now reload dump\n"),
    dump:reload(ID),
    timer:sleep(100),
    case Mode of
        hd ->
            3 = dump:top(ID);
        ram -> 3 = dump:top(ID)
    end,
    V2 = dump:get(2, ID).

