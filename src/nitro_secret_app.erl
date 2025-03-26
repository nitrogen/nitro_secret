%%%-------------------------------------------------------------------
%% @doc nitro_secret public API
%% @end
%%%-------------------------------------------------------------------

-module(nitro_secret_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nitro_secret_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
