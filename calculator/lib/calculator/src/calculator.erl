-module(calculator).

%% API
-export([add/1,
         substract/1,
         multiply/1,
         divide/1,
         get/0,
         set/1,
         reset/0]).

%% ===================================================================
%% API functions
%% ===================================================================

add(Num) when is_number(Num) ->
    gen_server:call(calculator, {add, Num}).

substract(Num) when is_number(Num) ->
    gen_server:call(calculator, {substract, Num}).

multiply(Num) when is_number(Num) ->
    gen_server:call(calculator, {multiply, Num}).

divide(Num) when is_number(Num) ->
    gen_server:call(calculator, {divide, Num}).

get() ->
    gen_server:call(calculator, get).


% Asynchronous calls

set(Num) when is_number(Num) ->
    gen_server:cast(calculator, {set, Num}).

reset() ->
    gen_server:cast(calculator, reset).