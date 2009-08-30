-module(scrumjet_api).

-compile(export_all).


task(ID, Headline, Description) when is_list(ID), is_list(Headline),
        is_list(Description) ->
    put_form("http://127.0.0.1:8000/tasks/" ++ mochiweb_util:quote_plus(ID),
        list_to_binary(mochiweb_util:urlencode(
            [{headline,Headline}, {description,Description}]))).

category(ID, Name) when is_list(ID), is_list(Name) ->
    put_form("http://127.0.0.1:8000/categories/" ++
        mochiweb_util:quote_plus(ID),
        list_to_binary(mochiweb_util:urlencode([{name,Name}]))).

board(ID, Name) when is_list(ID), is_list(Name) ->
    put_form("http://127.0.0.1:8000/boards/" ++ mochiweb_util:quote_plus(ID),
        list_to_binary(mochiweb_util:urlencode([{title,Name}]))).

category_task(ID, TaskID) when is_list(ID), is_list(TaskID) ->
    put_form("http://127.0.0.1:8000/category_tasks/" ++
        mochiweb_util:quote_plus(ID) ++ ";" ++
        mochiweb_util:quote_plus(TaskID), <<"">>).

board_category(ID, TaskID) when is_list(ID), is_list(TaskID) ->
    put_form("http://127.0.0.1:8000/board_categories/" ++
        mochiweb_util:quote_plus(ID) ++ ";" ++
        mochiweb_util:quote_plus(TaskID), <<"">>).

raw_put_form(URI, Body) when is_binary(Body) ->
    Headers = [],
    Options = [{max_keep_alive_length, 0}],
    ContentType = "application/x-www-urlencoded",
    Request = fun() ->
        http:request(put, {URI, Headers, ContentType, Body},
                            [{timeout, 3000}], Options)
    end,
    case Request() of
        {error, session_remotly_closed} ->
            {ok, _} = Request();
        {ok, _} = Success ->
            Success
    end.
put_form(URI, Body) when is_binary(Body) ->
    try raw_put_form(URI, Body)
    catch
    _:_ -> try raw_put_form(URI, Body)
        catch _:_ -> raw_put_form(URI, Body) end
    end;
put_form(URI, Body) when is_list(Body) ->
    put_form(URI, iolist_to_binary(Body)).

start() ->
    category("backlog", "Back Log"),
    category("sprint", "Sprint"),
    category("customer", "Customer Requests"),
    category("research", "Research and Development"),
    task("CR-767", "Acme Billings: Framistan Calibrator", "Acme Billings GmbH
wants us to add a user interface for calibrating their framistan product. It
should provide a knob for adjusting the..."),
    task("CR-1234", "Wolfram and Hart: Transaction Auditing", "Wolfram and Hart
wants us to add a user interface for auditing any business-to-business or
business-to-customer transactions. It is important to comply with Sarbanes-Oxley
and..."),
    task("RD-101", "Webmachine-based Product Catalog Prototype", "Build a
prototype product catalog application using Webmachine
http://bitbucket.org/justin/webmachine/wiki/Home . Track the time spent on
learning and implementing this feature using the..."),
    task("RD-526", "Nitrogen-based Product Catalog Prototype", "Build a
prototype product catalog application using Nitrogen http://nitrogenproject.com/
. Track the time spent on learning and implementing this feature using the..."),
    category_task("backlog", "RD-101"),
    category_task("backlog", "CR-1234"),
    category_task("sprint", "CR-767"),
    category_task("customer", "CR-767"),
    category_task("customer", "CR-1234"),
    category_task("research", "RD-101"),
    category_task("research", "RD-526"),
    board("development", "Development Dashboard"),
    board_category("development", "backlog"),
    board_category("development", "sprint").
