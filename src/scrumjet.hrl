% principle: relationships are not properties of records but rather are records themselves
-record(scrumjet_task, {id, mtime, ctime, headline, description}).
-record(scrumjet_board, {id, mtime, ctime, title}).
-record(scrumjet_category, {id, mtime, ctime, name}).
-record(scrumjet_category_task, {id, task_id}).
-record(scrumjet_board_category, {id, category_id}).