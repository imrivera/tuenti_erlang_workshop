-module(http_server_srv).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API
-export([start_link/1]).

%% State record definition
-record(state, {lsock,
                socket,
                method,
                uri,
                content_remaining = 0,
                body = [],
                headers = []
               }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([LSock]) ->
    State = #state{lsock = LSock, content_remaining = 0, body = []},

    {ok,        % Initialization ok
     State,     % Initial state of our server
     0}.        % Timeout, this will cause a call to handle_info with the timeout message


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({http, _Sock, {http_request, Method, {abs_path, Uri}, _}}, State) ->
    % Initial HTTP call
    inet:setopts(State#state.socket, [{active,once}]),
    {noreply, State#state{method = Method, uri = Uri}};
handle_info({http, _Sock, {http_header, _, Name, _, Value}}, State) ->
    inet:setopts(State#state.socket, [{active,once}]),
    {noreply, header(Name, Value, State)};
handle_info({http, _Sock, http_eoh}, #state{content_remaining = 0} = State) ->
    % Request without body
    {stop, normal, reply_to_request(State)};
handle_info({http, _Sock, http_eoh}, State) ->
    inet:setopts(State#state.socket, [{active, once}, {packet, raw}]),
    {noreply, State};
handle_info({tcp, _Sock, Data}, State) when is_binary(Data) ->
    ContentRem = State#state.content_remaining - byte_size(Data),
    Body = list_to_binary([State#state.body, Data]),
    NewState = State#state{body = Body,
                           content_remaining = ContentRem},

    case ContentRem > 0 of
        true ->
            inet:setopts(State#state.socket, [{active, once}]) ,
            {noreply, NewState};
        false ->
            {stop, normal, reply_to_request(NewState)}
    end;
handle_info(timeout, #state{lsock = LSock} = State) ->
    % Synchronous blocking call
    io:format("PID ~p: Waiting for client connection...~n", [self()]),
    {ok, Socket} = gen_tcp:accept(LSock),
    % Start a new child to accept a new connection
    http_server_sup:start_child(),
    % Activate socket to start receiving data
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}}.

terminate(Reason, _State) ->
    io:format("PID ~p terminated: ~p~n", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% internal functions
%% ===================================================================

header('Content-Length' = Name, Value, State) ->
    ContentLength = list_to_integer(binary_to_list(Value)),
    State#state{content_remaining = ContentLength,
                headers = [{Name, Value} | State#state.headers]};
header(Name, Value, State) ->
    State#state{headers = [{Name, Value} | State#state.headers]}.


reply_to_request(#state{method = 'GET', uri = <<"/hello">>} = State) ->
    io:format("Requesting hello...~n"),
    gen_tcp:send(State#state.socket, http_reply(200, "Hello World!\r\n")),
    State;

reply_to_request(#state{method = 'GET', uri = <<"/start_twitter">>} = State) ->
    io:format("Starting Twitter server...~n"),
    twitter:start_server(),
    gen_tcp:send(State#state.socket, http_reply(200, "Twitter server started\r\n")),
    State;

reply_to_request(#state{method = 'POST', uri = <<"/send">>, body = Body} = State) ->
    io:format("Sending \"~s\" to Twitter~n", [Body]),
    twitter:send_msg_all(Body),
    gen_tcp:send(State#state.socket, http_reply(200, "Message sent\r\n")),
    State;

reply_to_request(#state{method = 'POST', uri = <<"/echo">>, body = Body} = State) ->
    io:format("Echoing \"~s\"~n", [Body]),
    gen_tcp:send(State#state.socket, http_reply(200, Body)),
    State;

reply_to_request(State) ->
    io:format("Unrecognized request: ~p~n", [State]),
    gen_tcp:send(State#state.socket, http_reply(404, <<>>)),
    State.


http_reply(Code, Body) ->
    ContentBytes = iolist_to_binary(Body),
    Length = byte_size(ContentBytes),
    [io_lib:format("HTTP/1.1 ~s\r\nContent-Length: ~w\r\n\r\n", [response(Code), Length]), ContentBytes].

response(200) -> "200 OK";
response(404) -> "404 Not Found";
response(Code) -> integer_to_list(Code).