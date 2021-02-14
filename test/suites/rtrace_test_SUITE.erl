-module(rtrace_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). % Eunit macros for convenience

-export([all/0
        ,groups/0
        ,init_per_suite/1, end_per_suite/1
        ,init_per_group/2, end_per_group/2
         %%,init_per_testcase/2, end_per_testcase/2
        ]).

-export([
         rtrace_test/1
        ]).

%%=================================================
%% Test Setup
%%=================================================
all() ->
  [{group, rtrace_tests}].

groups() ->
  [{rtrace_tests, [], [rtrace_test]}].

init_per_suite(Config) ->
  application:ensure_all_started(rtrace),
  Config.

end_per_suite(_Config) ->
  application:stop(rtrace),
  ok.

init_per_group(rtrace_tests, Config) ->
  %% test setup for group rtrace_tests
  Config;

init_per_group(_, Config) ->
  Config.

end_per_group(rtrace_tests, _Config) ->
  %% teardown test setup for group rtrace_tests
  ok;

end_per_group(_Name, _Config) ->
  ok.


%%=================================================
%% Test Cases
%%=================================================
rtrace_test(_Config) ->
  ?assert(true).
