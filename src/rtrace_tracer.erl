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
-module(rtrace_tracer).

-behaviour(gen_server).

%% export API
-export([
         start_link/0,
         start_trace/3,
         clear_trace/0
        ]).

%% export gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {trace_id}).

-define(SERVER_NAME, ?MODULE).
%%====================================
%% API
%%====================================
start_link() ->
  gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

start_trace(Mod, Fun, Max) ->
  gen_server:call(?SERVER_NAME, {start_trace, Mod, Fun, Max}).

clear_trace() ->
  recon_trace:clear().

%%====================================
%% callbacks
%%====================================
init([]) ->
  {ok, #state{trace_id = undefined}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_info({io_request, From, ReplyAs, Request}, State) ->
  Reply = handle_io_request(Request, State),
  From ! {io_reply, ReplyAs, Reply},
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({start_trace, Mod, Fun, Max}, _From, State) ->
  Reply = do_start_trace(Mod, Fun, Max),
  NewState = update_tracer_state(Reply, State),
  {reply, Reply, NewState};

handle_call(_Msg, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.


%%====================================
%% private functions
%%====================================
handle_io_request({put_chars, Encoding, Module, Function, Args}, State) ->
  try
    Chars = erlang:apply(Module, Function, Args),
    handle_io_request({put_chars, Encoding, Chars}, State)
  catch
    _ : _ ->
      {error, {error, Function}}
  end;

handle_io_request({put_chars, Encoding, Chars}, State) ->
  %% encode chars into string
  Log = unicode:characters_to_binary(Chars, Encoding, unicode),

  %% write log
  #state{trace_id = UUID} = State,
  write_log(UUID, Log),
  ok;

handle_io_request(_, _) ->
  {error, not_implemented}.

write_log(UUID, {error, Encoded, _}) ->
  %% just write the encoded part to log
  rtrace_ets:write(UUID, Encoded);

write_log(UUID, {incomplete, Encoded, _}) ->
  %% just write the encoded part to log
  rtrace_ets:write(UUID, Encoded);

write_log(UUID, Log) ->
  rtrace_ets:write(UUID, Log).

do_start_trace(Mod, Fun, Max) ->
  Opts = [{scope, local}, {io_server, ?SERVER_NAME}],
  MatchSpec = [{'_', [], [{return_trace}]}],
  TR = recon_trace:calls({Mod, Fun, MatchSpec}, Max, Opts),
  case TR of
    0 ->
      {error, {function_not_found, {Mod, Fun}}};
    _ ->
      {ok, rtrace:uuid()}
  end.

update_tracer_state({ok, UUID}, State) ->
  State#state{trace_id = UUID};

update_tracer_state(_, State) ->
  State.
