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
-ifndef('__rtrace_hrl__').
-define('__rtrace_hrl__', true).

-define(RTRACE_PORT, 15000).
-define(RTRACE_HOST_IP, {0, 0, 0, 0}).
-define(RTRACE_LOG_TABLE, rtrace_log_table).
-define(RTRACE_DEFAULT_CALLS, 10).
-define(RTRACE_MAX_CALLS, 100).

-endif.
