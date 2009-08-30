% principle: relationships are not properties of records but rather are records themselves
-type time() :: {integer(), integer(), integer()}.
-record(scrumjet_task, {
    id :: string(),
    mtime :: time(),
    ctime :: time(),
    headline :: string(),
    description :: string()}).

-record(scrumjet_board, {
    id :: string(),
    mtime :: time(),
    ctime :: time(),
    title :: string()}).

-record(scrumjet_category, {
    id :: string(),
    mtime :: time(),
    ctime :: time(),
    name :: string()}).

-record(scrumjet_category_task, {
    id :: string(),
    task_id :: string()}).

-record(scrumjet_board_category, {
    id :: string(),
    category_id :: string()}).
