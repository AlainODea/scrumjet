-module(scrumjet_test_data).

-include("scrumjet.hrl").

-compile(export_all).

generate() ->
    ets:insert(scrumjet_task, #scrumjet_task{
        id= <<"CA-76">>,
        ctime= erlang:now(),
        mtime= erlang:now(),
        headline= <<"Bridgewater: Unidentified Members">>,
        description= <<"There are three unidentified members after the import. Any ideas?">>}),
    ets:insert(scrumjet_task, #scrumjet_task{
        id= <<"US-121">>,
        ctime= erlang:now(),
        mtime= erlang:now(),
        headline= <<"HAPO Community: Set up Users Extract">>,
        description= <<"These guys need the Users Extract. Speak with Rob for advice.">>}),
    ets:insert(scrumjet_task, #scrumjet_task{
        id= <<"Love-101">>,
        ctime= erlang:now(),
        mtime= erlang:now(),
        headline= <<"Watch Movie with Claudia">>,
        description= <<"Watch the freaking move and stop programming.">>}),
    ets:insert(scrumjet_task, #scrumjet_task{
        id= <<"Kids-233">>,
        ctime= erlang:now(),
        mtime= erlang:now(),
        headline= <<"Take the Whale Swimming">>,
        description= <<"Take Kaleb for his swimming lesson.">>}),
    ets:insert(scrumjet_category, #scrumjet_category{
        id= <<"family">>,
        ctime= erlang:now(),
        mtime= erlang:now(),
        name= <<"Family">>}),
    ets:insert(scrumjet_category_tasks, {<<"family">>, <<"Love-101">>}),
    ets:insert(scrumjet_category_tasks, {<<"family">>, <<"Kids-233">>}),
    ets:insert(scrumjet_category, #scrumjet_category{
        id= <<"backlog">>,
        ctime= erlang:now(),
        mtime= erlang:now(),
        name= <<"Back Log">>}),
    ets:insert(scrumjet_category_tasks, {<<"backlog">>, <<"CA-76">>}),
    ets:insert(scrumjet_category_tasks, {<<"backlog">>, <<"US-121">>}).