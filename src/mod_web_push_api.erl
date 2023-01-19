%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2023 Channel.me
%% @doc Web Push API for Zotonic

%% Copyright 2023 Channel.me
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

-module(mod_web_push_api).
-author("Maas-Maarten Zeeman <maas@channel.me>").

-mod_title("Web Push API").
-mod_description("Web Push API for Zotonic").
-mod_provides([]).
-mod_depends([]).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    init/1
]).

init(Context) ->
    ok.

