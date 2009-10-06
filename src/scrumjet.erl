%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2009 Alain O'Dea.

%% @doc TEMPLATE.

-module(scrumjet).
-author('Alain O\'Dea <alain.odea@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
    ok ->
        ok;
    {error, {already_started, App}} ->
        ok
    end.

%% @spec start() -> ok
%% @doc Start the scrumjet server.
start() ->
    scrumjet_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    mnesia:create_schema([node()]),
    ensure_started(mnesia),
    scrumjet_task:start(),
    scrumjet_category:start(),
    scrumjet_board:start(),
    application:start(scrumjet).

%% @spec stop() -> ok
%% @doc Stop the scrumjet server.
stop() ->
    Res = application:stop(scrumjet),
    application:stop(webmachine),
    application:stop(crypto),
    application:stop(mnesia),
    Res.
