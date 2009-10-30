-module(scrumjet_test_datastore).

-include_lib("eunit/include/eunit.hrl").
-include("scrumjet.hrl").

% 1. Exercise the happy path code
%    - Set up simple pre-test state of system under test (SUT)
%    - Exercise SUT by calling the method being tested.
setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    scrumjet_datastore:start().

cleanup(_Pid) -> ok.

happy_path_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun happy_path_generator/1
    }.

happy_path_generator(_Pid) ->
    {timeout, 1, [
        fun() -> scrumjet_datastore:store(
            #scrumjet_category{id="family", name="Family Stuff"}) end,
        fun() -> scrumjet_datastore:find(#scrumjet_category{id="family"}) end
    ]}.

% 2. Verify direct outputs of happy path
%    - Call Assertion Methods on SUT responses
%    - Call Assertion Methods on post-test state
verify_happy_path_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun verify_happy_path_generator/1
    }.

verify_happy_path_generator(_Pid) ->
    {timeout, 1, [
        ?_assertEqual(ok, scrumjet_datastore:store(
            #scrumjet_category{id="family", name="Family Stuff"})),
        ?_assertMatch([#scrumjet_category{id="family", name="Family Stuff"}],
            scrumjet_datastore:find(#scrumjet_category{id="family"}))
    ]}.

% 3. Verify Alternate Paths
%    - Vary SUT method arguments
%    - Vary pre-test state of SUT
verify_alternate_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun verify_alternate_generator/1
    }.

verify_alternate_generator(_Pid) ->
    {timeout, 1, [
        ?_assertMatch([], scrumjet_datastore:find(
            #scrumjet_category{id="family"})),
        ?_assertMatch([], scrumjet_datastore:find(
            #scrumjet_category{id="work"})),
        ?_assertEqual(ok, scrumjet_datastore:store(
            #scrumjet_category{id="family", name="Family Stuff"})),
        ?_assertEqual(ok, scrumjet_datastore:store(
            #scrumjet_category{id="work", name="Work"})),
        ?_assertMatch([#scrumjet_category{id="family", name="Family Stuff"}],
            scrumjet_datastore:find(
                #scrumjet_category{id="family"})),
        ?_assertMatch([#scrumjet_category{id="work", name="Work"}],
            scrumjet_datastore:find(
                #scrumjet_category{id="work"}))
    ]}.

% 4. Control Indirect Inputs of SUT via Test Stub
%    - Verify indirect outputs
%    - Use Mock Objects or Test Spys to intercept and verify outgoing calls

% 5. Optimize Execution & Maintainability
%    - Make the tests run faster
%    - Make tests easy to understand and maintain
%    - Design the SUT for testability
%    - Reduce Risk of Missed Bugs
