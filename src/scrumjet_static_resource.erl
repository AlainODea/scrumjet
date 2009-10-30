%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @author Alain O'Dea <alain.odea@gmail.com>
%% @copyright 2008-2009 Basho Technologies, Inc.

-module(scrumjet_static_resource).
-export([init/1]).
-export([allowed_methods/2,
     resource_exists/2,
     last_modified/2,
     content_types_provided/2,
     provide_content/2,
     generate_etag/2]).

-record(context, {root,response_body=undefined,metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(ConfigProps) ->
    {root, Root} = proplists:lookup(root, ConfigProps),
    {ok, #context{root=Root}}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

file_path(Context, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    filename:join([Context#context.root, RelName]).

file_exists(Context, Name) ->
    NamePath = file_path(Context, Name),
    case filelib:is_regular(NamePath) of
    true ->
        {true, NamePath};
    false ->
        false
    end.

resource_exists(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    case file_exists(Context, Path) of
    {true, _} ->
        {true, ReqData, Context};
    _ ->
            case Path of
                "p" -> {true, ReqData, Context};
                _ -> {false, ReqData, Context}
            end
    end.

maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
    undefined ->
        case file_exists(Context, Path) of
        {true, FullPath} ->
            {ok, Value} = file:read_file(FullPath),
            {true, Context#context{response_body=Value}};
        false ->
            {false, Context}
        end;
    _Body ->
        {true, Context}
    end.

content_types_provided(ReqData, Context) ->
    CT = webmachine_util:guess_mime(wrq:disp_path(ReqData)),
    {[{CT, provide_content}], ReqData,
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

provide_content(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
    {true, NewContext} ->
        Body = NewContext#context.response_body,
        {Body, ReqData, Context};
    {false, NewContext} ->
        {error, ReqData, NewContext}
    end.

last_modified(ReqData, Context) ->
    {true, FullPath} = file_exists(Context,
                                   wrq:disp_path(ReqData)),
    LMod = filelib:last_modified(FullPath),
    {LMod, ReqData, Context#context{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|Context#context.metadata]}}.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:sha(Body))).

generate_etag(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, BodyContext} ->
            ETag = hash_body(BodyContext#context.response_body),
            {ETag, ReqData,
             BodyContext#context{metadata=[{etag,ETag}|
                                           BodyContext#context.metadata]}};
        _ ->
            {undefined, ReqData, Context}
    end.
