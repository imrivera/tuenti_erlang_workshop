-module(echo_server_listener).

%% API
-export([start_link/0]).

-define(PORT, 5555).


start_link() ->
    NewPid = spawn_link(fun do_listen/0),
    {ok, NewPid}.


do_listen() ->
    case gen_tcp:listen(?PORT, [{active, true}, {packet, 0}, {reuseaddr, true}, binary]) of
        {ok, ListenSocket} ->

            case gen_tcp:accept(ListenSocket) of
                {ok, Socket} ->
                    io:format("Process PID ~p listening to port ~p~n", [self(), ?PORT]),
                    gen_tcp:close(ListenSocket),
                    loop(Socket);

                Other ->
                    io:format("gen_tcp:accept failed: ~p~n", [Other])
            end;

        {error, Reason} ->
            io:format("gen_tcp:listen failed: ~p~n", [Reason])
    end.


loop(Socket) ->
    receive
      {tcp, Socket, <<"bye bye", _/binary>>} ->
          gen_tcp:send(Socket, <<"Goodbye. Have a nice day\n">>),
          gen_tcp:close(Socket),
          io:format("Closing socket ~p. Terminating PID ~p~n", [Socket, self()]);

      {tcp, Socket, Data} ->
          gen_tcp:send(Socket, Data),
          loop(Socket);

      {tcp_closed, Socket} ->
          io:format("Socket ~p closed by client. Terminating PID ~p~n", [Socket, self()])
    end.




