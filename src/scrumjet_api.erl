-module(scrumjet_api).

-compile(export_all).

task_example() ->
    put_form("http://127.0.0.1:8000/tasks/US-191",
<<"headline=TDECU%20Users%20Extract&description=TDECU%20needs%20the%20Users%20extract%20set%20up%20on%20their%20site">>).

task(ID, Headline, Description) when is_list(ID), is_list(Headline), is_list(Description) ->
    put_form("http://127.0.0.1:8000/tasks/" ++ mochiweb_util:quote_plus(ID),
        list_to_binary(mochiweb_util:urlencode(lists:zip([<<"headline">>,<<"description">>],[Headline,Description])))).

put_form(URI, Body) when is_list(URI), is_binary(Body) ->
    Headers = [],
    Options = [],
    ContentType = "application/x-www-urlencoded",
    http:request(put, {URI, Headers, ContentType, Body}, [{timeout, 3000}], Options);
put_form(URI, Body) when is_list(URI), is_list(Body) ->
    put_form(URI, iolist_to_binary(Body)).