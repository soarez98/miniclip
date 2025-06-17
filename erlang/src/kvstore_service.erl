-module(kvstore_service).
-behaviour(gen_server).

-export([start_link/0, start_server/0, stop_server/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("kvstore_pb.hrl").

-record(state, {socket, dynamodb, kms_key_id}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_server() ->
    {ok, Port} = application:get_env(kvstore, port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]),
    spawn(fun() -> accept_loop(ListenSocket) end),
    ok.

stop_server() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    {ok, KMSKeyId} = application:get_env(kvstore, kms_key_id),
    ok = erlcloud:start(),
    {ok, #state{socket = undefined, dynamodb = erlcloud_ddb2, kms_key_id = KMSKeyId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    try
        Decoded = kvstore_pb:decode_msg(Data, 'req_envelope'),
        Response = process_message(Decoded, State),
        Encoded = kvstore_pb:encode_msg(Response),
        gen_tcp:send(Socket, Encoded),
        {noreply, State#state{socket = Socket}}
    catch
        error:Reason ->
            error_logger:error_msg("Decode error: ~p~n", [Reason]),
            {noreply, State}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    {noreply, State#state{socket = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

accept_loop(ListenSocket) ->
    {ok} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> accept_loop(ListenSocket) end),
    ok.

process_message(#'req_envelope'{type = 'set_request_t', set_req = SetReq}, State) ->
    #'set_request'{req = #'data'{key = Key, value = Value}} = SetReq,
    case store_data(Key, Value, State) of
        ok ->
            #'req_envelope'{type = 'set_response_t', set_resp = #'set_response'{error = 'ok'}};
        {error, _} ->
            #'req_envelope'{type = 'set_response_t', set_resp = #'set_response'{error = 'internal'}}
    end;
process_message(#'req_envelope'{type = 'get_request_t', get_req = #'get_request'{key = Key}}, State) ->
    case retrieve_data(Key, State) of
        {ok, Value} ->
            #'req_envelope'{type = 'get_response_t', get_resp = #'get_response'{error = 'ok', req = #'data'{key = Key, value = Value}}};
        {error, not_found} ->
            #'req_envelope'{type = 'get_response_t', get_resp = #'get_response'{error = 'not_found'}};
        {error, _} ->
            #'req_envelope'{type = 'get_response_t', get_resp = #'get_response'{error = 'internal'}}
    end.

store_data(Key, Value, #state{dynamodb = DDB, kms_key_id = KMSKeyId}) ->
    try
        EncryptedValue = encrypt_data(Value, KMSKeyId),
        Attrs = [{<<"key">>, {s, Key}}, {<<"value">>, {b, EncryptedValue}}],
        case DDB:put_item(<<"KVStore">>, Attrs, [{return_values, none}]) of
            {ok, _} -> ok;
            {error, Error} ->
                error_logger:error_msg("DynamoDB put error: ~p~n", [Error]),
                {error, internal}
        end
    catch
        CatchError:Reason ->
            error_logger:error_msg("Store error: ~p:~p~n", [CatchError, Reason]),
            {error, internal}
    end.

retrieve_data(Key, #state{dynamodb = DDB}) ->
    try
        case DDB:get_item(<<"KVStore">>, [{<<"key">>, {s, Key}}]) of
            {ok, Item} ->
                case proplists:get_value(<<"value">>, Item) of
                    {b, EncryptedValue} ->
                        {ok, decrypt_data(EncryptedValue)};
                    undefined ->
                        {error, not_found}
                end;
            {error, Error} ->
                error_logger:error_msg("DynamoDB get error: ~p~n", [Error]),
                {error, internal}
        end
    catch
        CatchError:Reason ->
            error_logger:error_msg("Retrieve error: ~p:~p~n", [CatchError, Reason]),
            {error, internal}
    end.

encrypt_data(Data, KMSKeyId) ->
    {ok, Result} = erlcloud_kms:encrypt(KMSKeyId, Data),
    proplists:get_value(<<"CiphertextBlob">>, Result).

decrypt_data(EncryptedData) ->
    {ok, Result} = erlcloud_kms:decrypt(EncryptedData),
    proplists:get_value(<<"Plaintext">>, Result).