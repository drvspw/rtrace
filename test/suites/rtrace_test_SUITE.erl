%%-------------------------------------------------------------------------------------------
%% Copyright (c) 2021 Venkatakumar Srinivasan
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Venkatakumar Srinivasan
%% @since February 14, 2021
%%
%%-------------------------------------------------------------------------------------------
-module(rtrace_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). % Eunit macros for convenience

-export([all/0
        ,groups/0
        ,init_per_suite/1, end_per_suite/1
        ,init_per_group/2, end_per_group/2
        ,init_per_testcase/2, end_per_testcase/2
        ]).

-export([
         rtrace_test/1,
         rtrace_no_mfa_test/1,
         rtrace_trace_api_test/1,
         rtrace_trace_api_error_test/1,
         rtrace_logs_api_test/1
        ]).

-include("rtrace.hrl").

%%=================================================
%% Test Setup
%%=================================================
all() ->
  [{group, rtrace_tests}].

groups() ->
  [
   {rtrace_tests, [], [
                       rtrace_test, rtrace_no_mfa_test,
                       rtrace_trace_api_test, rtrace_trace_api_error_test,
                       rtrace_logs_api_test
                      ]
   }
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(rtrace),
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

init_per_testcase(_Name, Config) ->
  Config.

end_per_testcase(_Name, _Config) ->
  rtrace_tracer:clear_trace(),
  ok.

%%=================================================
%% Test Cases
%%=================================================
rtrace_test(_Config) ->
  %% start trace
  {ok, UUID} = rtrace:trace_calls(rtrace, priv_dir),

  %% call the function. This would generate trace logs
  rtrace:priv_dir(),

  %% wait for a second to get trace logs populated
  timer:sleep(1000),

  %% get logs
  Logs = rtrace_ets:logs(UUID),
  ?assertEqual(true, length(Logs) >= 1).

rtrace_no_mfa_test(_Config) ->
  {error, Reason} = rtrace:trace_calls(map, get),
  {function_not_found, {map, get}} = Reason,
  ?assert(true).

rtrace_trace_api_test(_Config) ->
  Url = api_url("/trace"),
  Headers = [],
  TraceRequest = jsone:encode(#{mod => 'maps', 'fun' => get}),

  {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} =
    httpc:request(post, {Url, Headers, "application/json", TraceRequest}, [], []),
  ?assertEqual(200, Status),

  %% decode body
  UUID = jsone:decode(list_to_binary(Body)),
  ?assertEqual(36, size(UUID)),

  maps:get(q, #{}, u),
  ?assert(true).

rtrace_trace_api_error_test(_Config) ->
  Url = api_url("/trace"),
  Headers = [],
  TraceRequest = jsone:encode(#{mod => 'map', 'fun' => get}),

  {ok, {{_Version, Status, _ReasonPhrase}, _Headers, _Body}} =
    httpc:request(post, {Url, Headers, "application/json", TraceRequest}, [], []),
  ?assertEqual(400, Status),
  ?assert(true).

rtrace_logs_api_test(_Config) ->
  Url = api_url("/trace"),
  Headers = [],
  TraceRequest = jsone:encode(#{mod => rtrace, 'fun' => priv_dir}),

  {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} =
    httpc:request(post, {Url, Headers, "application/json", TraceRequest}, [], []),
  ?assertEqual(200, Status),

  %% decode body to get UUID
  UUID = jsone:decode(list_to_binary(Body)),

  %% Now call function to generate the trace
  rtrace:priv_dir(),
  rtrace:priv_dir(),

  %% wait for a second to get trace logs populated
  timer:sleep(1000),

  %% call logs api to get logs
  LogsUrl = api_url("/logs/" ++ binary_to_list(UUID)),
  LogsHeaders = [{"accept", "application/json"}],

  {ok, {{_LogsVersion, LogsStatus, _LogsReasonPhrase}, _LogsHeaders, LogsBody}} =
    httpc:request(get, {LogsUrl, LogsHeaders}, [], []),
  ?assertEqual(200, LogsStatus),

  %% decode logs api response
  Logs = jsone:decode(list_to_binary(LogsBody)),
  ?assertEqual(true, length(Logs) >= 1).

%%=================================================
%% private functions
%%=================================================
api_url(Path) ->
  Port = application:get_env(rtrace, http_port, ?RTRACE_PORT),
  "http://localhost:" ++ integer_to_list(Port) ++ Path.
