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
-module(rtrace_http).

-include_lib("kernel/include/logger.hrl").
-include_lib("elli/include/elli.hrl").

-include("rtrace.hrl").

-behaviour(elli_handler).


%% elli callbacks
-export([
         handle/2,
         handle_event/3
        ]).

-define(ATOM(X), binary_to_existing_atom(X, latin1)).
-define(INDEX_PAGE, <<"index.html">>).
-define(API_VSN, <<"v1">>).
%%===========================================================================
%% elli Callbacks
%%===========================================================================
handle(Req, _Args) ->
  handle(Req#req.method, elli_request:path(Req), Req).

handle_event(_Event, _Data, _Args) ->
  ok.

%%===========================================================================
%% private functions
%%===========================================================================
handle('POST', [<<"api">>, ?API_VSN, <<"trace">>], Req) ->
  %% Get the request body and parse json
  ReqBody = elli_request:body(Req),
  Request = parse_trace_request(ReqBody),

  %% start trace
  Result = handle_trace_request(Request),

  %% generate response
  generate_response(Result);

handle('GET', [<<"api">>, ?API_VSN, <<"logs">>, UUID], _Req) ->
  %% lookup logs from ets table
  LogItems = rtrace_ets:logs(UUID),
  Logs = [L || {_, L} <- LogItems],
  {200, [], jsone:encode(Logs)};

handle(_, [<<"api">>|_], _Req) ->
  %% api path not found
  {404, [], <<"">>};

handle('GET', [], Req) ->
  handle('GET', [?INDEX_PAGE], Req);

handle('GET', [?INDEX_PAGE] = Path, Req) ->
  serve_file(Path, Req);

handle('GET', [<<"css">>|_] = Path, Req) ->
  serve_file(Path, Req);

handle('GET', [<<"js">>|_] = Path, Req) ->
  serve_file(Path, Req);

handle(_, _, Req) ->
  %% serve index.html and hope the client js displays the error message
  serve_file([?INDEX_PAGE], Req).

handle_trace_request({Mod, Fun, Calls}) ->
  try
    rtrace:trace_calls(Mod, Fun, Calls)

  catch
    E : R : S ->
      ?LOG_ERROR("Handle Trace Request Error: ~p, Reason: ~p~n~p", [E, R, S]),
      {error, {not_started, {Mod, Fun}}}
  end.

generate_response({ok, UUID}) ->
  {200, [], jsone:encode(UUID)};

generate_response({error, {Code, {Mod, Fun}}}) ->
  Text = error_text(Code),
  Resp = jsone:encode(#{error => Text,
                        mod => Mod,
                        <<"fun">> => Fun
                        }),
  {400, [], Resp}.

error_text(function_not_found) ->
  <<"function not found">>;
error_text(not_started) ->
  <<"unable to start trace">>;
error_text(_) ->
  <<"generic error">>.

parse_trace_request(ReqBody) ->
  TraceRequest = jsone:decode(ReqBody),
  #{<<"mod">> := Mod, <<"fun">> := Fun} = TraceRequest,
  Calls = maps:get(<<"calls">>, TraceRequest, ?RTRACE_DEFAULT_CALLS),
  {?ATOM(Mod), ?ATOM(Fun), ceil_calls(Calls)}.

ceil_calls(Calls) when Calls > ?RTRACE_MAX_CALLS ->
  ?RTRACE_MAX_CALLS;
ceil_calls(Calls) when Calls =< 0 ->
  ?RTRACE_DEFAULT_CALLS;
ceil_calls(Calls) ->
  Calls.

serve_file(Path, _Req) ->
  FilePath = web_file(Path),
  {ok, [], {file, FilePath}}.

web_file(Path) ->
  File = filename:join([rtrace:priv_dir(), <<"web">>|Path]),
  case filelib:is_regular(File) of
    true ->
      File;
    _ ->
      filename:join([rtrace:priv_dir(), <<"web">>, ?INDEX_PAGE])
  end.
