-module(calculator_srv).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API
-export([start_link/0]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    true = register(calculator, self()),
    io:format("Calculator server started ~p~n", [self()]),
    {ok, 0}.

handle_call({add, Num}, _From, State) ->
    NewState = State + Num,
    {reply, NewState, NewState};
handle_call({substract, Num}, _From, State) ->
    NewState = State - Num,
    {reply, NewState, NewState};
handle_call({multiply, Num}, _From, State) ->
    NewState = State * Num,
    {reply, NewState, NewState};
handle_call({divide, Num}, _From, State) ->
    NewState = State / Num,
    {reply, NewState, NewState};
handle_call(get, _From, State) ->
    {reply, State, State};
handle_call(_, _From, State) ->
    {reply, undefined, State}.

handle_cast({set, Num}, _State) ->
    {noreply, Num};
handle_cast(reset, _State) ->
    {noreply, 0};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Whatever, State) ->
    io:format("Unknown message received: ~p~n", [Whatever]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("PID ~p terminated: ~p~n", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
