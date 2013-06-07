%% -------------------------------------------------------------------
%% Copyright (c) 2013 Xujin Zheng (zhengxujin@adsage.com)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% -------------------------------------------------------------------

-module(util_misc).

-export([load_app_env/1, load_app_env/2]).

load_app_env(EnvFile) ->
    case application:get_application() of 
        {ok, AppName} ->
            load_app_env(AppName, EnvFile);
        undefined ->
            {error, unknown_application}
    end.

load_app_env(AppName, EnvFile) ->
    case file:consult(EnvFile) of
        {ok, Terms} ->
            Envs         = proplists:get_value(AppName, Terms),
            OriginalEnvs = application:get_all_env(AppName),
            NewEnvKeys   = 
            [ begin 
                ok = application:set_env(AppName, Par, Val),
                Par
              end
              ||{Par, Val} <-Envs],
            [ ok = application:unset_env(AppName, Par) 
              ||{Par , _Val} <-OriginalEnvs, 
                lists:member(Par, NewEnvKeys) =:= false],
            ok;
        Error ->
            Error
    end.