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
-module(rtrace).

%% api
-export([
         uuid/0,

         trace_calls/2,
         trace_calls/3,

         priv_dir/0
        ]).

%% Application behaviour and callbacks
-behaviour(application).

-export([start/2, stop/1]).

%% supervisor behaviour and callbacks
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").
-include("rtrace.hrl").

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% @doc rtrace public API
%% @end
%%%-------------------------------------------------------------------
trace_calls(Mod, Fun) ->
  trace_calls(Mod, Fun, ?RTRACE_DEFAULT_CALLS).

trace_calls(Mod, Fun, Max) ->
  rtrace_tracer:start_trace(Mod, Fun, Max).

uuid() ->
  list_to_binary(uuidv4:to_string(uuidv4:new())).

priv_dir() ->
  case code:priv_dir(?MODULE) of
    {error, bad_name} ->
      logger:info("Couldn't find priv dir for the application, using ./priv~n"),
      "./priv";
    PrivDir -> filename:absname(PrivDir)
  end.

%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc rtrace application callbacks
%% @end
%%%-------------------------------------------------------------------
start(_StartType, _StartArgs) ->
  {ok, Pid} = ?MODULE:start_link(),

  {ok, Pid}.

stop(_State) ->
  ok.
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc rtrace top level supervisor.
%% @end
%%%-------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
  ?LOG_INFO("Starting a new supervisor"),
  RestartStrategy = {one_for_one, 4, 3600},

  %% ets
  Ets = #{id => rtrace_ets,
          start => {rtrace_ets, start_link, []},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [rtrace_ets]
         },

  %% http listener
  Port = application:get_env(?MODULE, http_port, ?RTRACE_PORT),
  HostIp = application:get_env(?MODULE, http_host_ip, ?RTRACE_HOST_IP),
  Options = [
             {ip, HostIp},
             {port, Port},
             {callback, rtrace_http}
            ],
  HttpListener = #{id => rtrace_http,
                   start => {elli, start_link, [Options]},
                   restart => permanent,
                   shutdown => 2000,
                   type => worker,
                   modules => [elli]
                  },

  %% tracer
  Tracer = #{ id => rtrace_tracer,
              start => {rtrace_tracer, start_link, []},
              restart => permanent,
              shutdown => 2000,
              type => worker,
              modules => [rtrace_tracer]},

  Children = [Ets, HttpListener, Tracer], %% [ child_spec() ]
  {ok, {RestartStrategy, Children}}.

%%--------------------------------------------------------------------

%%=========================================================================
%% Private functions
%%=========================================================================
