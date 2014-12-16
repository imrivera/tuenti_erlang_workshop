-module(http_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

start_child() ->
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    SockOpts = [binary,             % Receive binary data instead of normal strings
                {active, false},    % Don't start receiving data until I'm ready
                {packet, http_bin}, % We expect a HTTP request
                {reuseaddr, true}],
    {ok, LSock} = gen_tcp:listen(Port, SockOpts),
    Server = {http_server_srv, {http_server_srv, start_link, [LSock]},
              temporary, brutal_kill, worker, [http_server_srv]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

