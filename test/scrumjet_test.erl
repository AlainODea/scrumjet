-module(scrumjet_test).

-compile(export_all).

create_task() ->
    http:request(put, {"http://127.0.0.1:8000/tasks/US-191", [], "application/x-www-urlencoded",
<<"headline=TDECU%20Users%20Extract&description=TDECU%20needs%20the%20Users%20extract%20set%20up%20on%20their%20site\n\n">>},
[{timeout, 3000}],
[]).