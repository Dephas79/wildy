%%%-------------------------------------------------------------------
%% @doc wildy public API
%% @end
%%%-------------------------------------------------------------------

-module(wildy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    wildy_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
