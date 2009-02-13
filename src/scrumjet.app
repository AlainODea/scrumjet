{application, scrumjet,
 [{description, "scrumjet"},
  {vsn, "0.18"},
  {modules, [
    scrumjet,
    scrumjet_app,
    scrumjet_sup,
    scrumjet_deps,
    scrumjet_resource
  ]},
  {registered, []},
  {mod, {scrumjet_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
