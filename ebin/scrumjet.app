{application, scrumjet,
 [{description, "ScrumJet: RESTful task tracking experiment built with webmachine"},
  {vsn, "0.1"},
  {modules, [
    scrumjet
    scrumjet_api
    scrumjet_app
    scrumjet_board_category
    scrumjet_board_category_resource
    scrumjet_board_resource
    scrumjet_boards_resource
    scrumjet_categories_resource
    scrumjet_category_resource
    scrumjet_category_task
    scrumjet_category_task_resource
    scrumjet_datastore
    scrumjet_deps
    scrumjet_html
    scrumjet_json
    scrumjet_mnesia
    scrumjet_resource
    scrumjet_static_resource
    scrumjet_sup
    scrumjet_task_resource
    scrumjet_tasks_resource
    scrumjet_test_datastore
    scrumjet_uri
  ]},
  {registered, [scrumjet_task]},
  {mod, {scrumjet_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, mnesia]}]}.
