-module(http_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(PORT, 7777).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case http_server_sup:start_link(?PORT) of
        {ok, Pid} ->
            http_server_sup:start_child(),  % Spawns initial handler
            io:format("HTTP started, listening on port ~p~n", [?PORT]),
            {ok, Pid};
        Other ->
            io:format("There was a problem starting the HTTP server: ~p~n", [Other]),
            {error, Other}
    end.

stop(_State) ->
    ok.
