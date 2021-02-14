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
-module(rtrace_ets).

-behaviour(gen_server).

%% export API
-export([start_link/0,
         stop/0
]).

%% log table API
-export([
         write/2,
         logs/1
        ]).

%% export gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-include("rtrace.hrl").

-define(SERVER_NAME, ?MODULE).

%%====================================
%% API
%%====================================
start_link() ->
  gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER_NAME, stop).

write(UUID, Log) ->
  ets:insert(?RTRACE_LOG_TABLE, {UUID, Log}).

logs(UUID) ->
  ets:lookup(?RTRACE_LOG_TABLE, UUID).

%%====================================
%% callbacks
%%====================================
init([]) ->
  %% create the trace log ets table
  ets:new(?RTRACE_LOG_TABLE, [bag, named_table, public]),
  {ok, #state{}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_info(_Msg, State) ->
  {noreply, State}.


handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.
