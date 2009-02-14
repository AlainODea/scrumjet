-module(scrumjet_test_data).

-include("scrumjet.hrl").

-compile(export_all).

generate() ->
    ets:insert(scrumjet_task, #scrumjet_task{
        id= <<"CA-76">>,
        headline= <<"Bridgewater: Unidentified Members">>,
        description= <<"There are three unidentified members after the import. Any ideas?">>}),
    ets:insert(scrumjet_task, #scrumjet_task{
        id= <<"US-121">>,
        headline= <<"HAPO Community: Set up Users Extract">>,
        description= <<"These guys need the Users Extract. Speak with Rob for advice.">>}),
    ets:insert(scrumjet_category, #scrumjet_category{
        id= <<"backlog">>,
        name= <<"Back Log">>}),
    ets:insert(scrumjet_category_tasks, {<<"backlog">>, <<"CA-76">>}),
    ets:insert(scrumjet_category_tasks, {<<"backlog">>, <<"US-121">>}).
