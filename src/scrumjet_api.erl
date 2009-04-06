-module(scrumjet_api).

-compile(export_all).

task_example() ->
    task("WD-101", "Build Web Site", "Bob's Hardware needs a new web site.").

category(ID, Name) when is_list(ID), is_list(Name) ->
    put_form("http://127.0.0.1:8000/categories/" ++ mochiweb_util:quote_plus(ID),
        list_to_binary(mochiweb_util:urlencode([{name,Name}]))).

task(ID, Headline, Description) when is_list(ID), is_list(Headline), is_list(Description) ->
    put_form("http://127.0.0.1:8000/tasks/" ++ mochiweb_util:quote_plus(ID),
        list_to_binary(mochiweb_util:urlencode([{headline,Headline}, {description,Description}]))).

category_task(ID, TaskID) when is_list(ID), is_list(TaskID) ->
    put_form("http://127.0.0.1:8000/category_tasks/" ++ mochiweb_util:quote_plus(ID) ++ ";" ++ mochiweb_util:quote_plus(TaskID), <<"">>).

put_form(URI, Body) when is_list(URI), is_binary(Body) ->
    Headers = [],
    Options = [],
    ContentType = "application/x-www-urlencoded",
    http:request(put, {URI, Headers, ContentType, Body}, [{timeout, 3000}], Options);
put_form(URI, Body) when is_list(URI), is_list(Body) ->
    put_form(URI, iolist_to_binary(Body)).

test() ->
    scrumjet_api:category("backlog", "Back Log"),
    scrumjet_api:category("sprint", "Sprint"),
    scrumjet_api:category("customer", "Customer Requests"),
    scrumjet_api:category("research", "Research and Development"),
    scrumjet_api:task("CR-767", "Acme Billings: Framistan Calibrator", "Acme Billings GmbH wants us to add a user interface for calibrating their framistan product. It should provide a knob for adjusting the..."),
    scrumjet_api:task("CR-1234", "Wolfram and Hart: Transaction Auditing", "Wolfram and Hart wants us to add a user interface for auditing any business-to-business or business-to-customer transactions. It is important to comply with Sarbanes-Oxley and..."),
    scrumjet_api:task("RD-101", "webmachine-based Product Catalog Prototype", "Build a prototype product catalog application using webmachine. Track the time spent on learning and implementing this feature using the..."),
    scrumjet_api:category_task("backlog", "RD-101"),
    scrumjet_api:category_task("backlog", "CR-1234"),
    scrumjet_api:category_task("sprint", "CR-767"),
    scrumjet_api:category_task("customer", "CR-767"),
    scrumjet_api:category_task("customer", "CR-1234"),
    scrumjet_api:category_task("research", "RD-101").
