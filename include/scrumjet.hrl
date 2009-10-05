% principle: relationships are not properties of records but rather are records themselves
-type time() :: {integer(), integer(), integer()}.
-record(scrumjet_task, {
    id :: string(),
    mtime :: time(),
    ctime :: time(),
    headline :: binary(),
    description :: binary()}).

-record(scrumjet_board, {
    id :: string(),
    mtime :: time(),
    ctime :: time(),
    title :: binary()}).

-record(scrumjet_category, {
    id :: string(),
    mtime :: time(),
    ctime :: time(),
    name :: binary()}).

-record(scrumjet_category_task, {
    id :: string(),
    task_id :: string()}).

-record(scrumjet_board_category, {
    id :: string(),
    category_id :: string()}).

-type sj_record() :: #scrumjet_task{}|#scrumjet_category{}|#scrumjet_board{}.
