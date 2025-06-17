-module(kvstore_client).
-export([test/0]).

-include("kvstore_pb.hrl").

test() ->
    {ok, Socket} = gen_tcp:connect("localhost", 8080, [binary, {packet, 0}]),
    % Test SET
    SetData = #'data'{key = "test_key", value = "test_value"},
    SetReq = #'req_envelope'{type = 'set_request_t', set_req = #'set_request'{req = SetData}},
    ok = gen_tcp:send(Socket, kvstore_pb:encode_msg(SetReq)),
    receive
        {tcp, Socket, Data} ->
            Decoded = kvstore_pb:decode_msg(Data, 'req_envelope'),
            io:format("Set Response: ~p~n", [Decoded#'req_envelope'.set_resp])
    after 5000 ->
        io:format("Set timeout~n")
    end,
    % Test GET
    GetReq = #'req_envelope'{type = 'get_request_t', get_req = #'get_request'{key = "test_key"}},
    ok = gen_tcp:send(Socket, kvstore_pb:encode_msg(GetReq)),
    receive
        {tcp, Socket, Data2} ->
            Decoded2 = kvstore_pb:decode_msg(Data2, 'req_envelope'),
            io:format("Get Response: ~p~n", [Decoded2#'req_envelope'.get_resp])
    after 5000 ->
        io:format("Get timeout~n")
    end,
    gen_tcp:close(Socket).