%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.

%% @doc Callbacks for the scrumjet application.

-module(scrumjet_app).
-author('Alain O\'Dea <alain.odea@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for scrumjet.
start(_Type, _StartArgs) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    scrumjet_deps:ensure(),
    scrumjet_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for scrumjet.
stop(_State) ->
    ok.
