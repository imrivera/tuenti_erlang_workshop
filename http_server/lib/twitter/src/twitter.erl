-module(twitter).

%% API
-export([start_server/0,
         send_msg/2,
         send_msg_all/1,
         list_twitter_pids/0]).

start_server() ->
    spawn(fun() ->
              register(twitter, self()),
              loop_msg()
          end).

loop_msg() ->
    receive
        {msg, From, Msg} when is_atom(From) ->
            io:format("Tweet from ~p: ~s~n", [From, Msg]),
            loop_msg();
        die ->
            io:format("Twitter server terminating~n");
        Other ->
            io:format("Invalid message received: ~p~n", [Other]),
            loop_msg()
    end.

send_msg(Node, Msg) ->
    {twitter, Node} ! {msg, node(), Msg}.

send_msg_all(Msg) ->
    [send_msg(Node, Msg) || Node <- [node() | nodes()]].

list_twitter_pids() ->
    lists:map(fun(Node) ->
                  RemotePid = rpc:call(Node, erlang, whereis, [twitter]),
                  io:format("~p~n", [[Node, RemotePid]])
              end,
              nodes()).