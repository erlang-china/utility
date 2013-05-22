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

-module(util_time).

-export([string_time/0,
         string_date/0,
         string_now/0,
         time_diff/2,
         string_tick/0, 
         long_tick/0,
         string_now_micro/0]).

string_time()->
    {{_,_,_},{HH, MM, SS}}= erlang:localtime(),
    lists:flatten(io_lib:format("~2..0w~2..0w~2..0w",[HH, MM, SS])).

string_date()->
    {{YYYY,MM,DD},{_,_,_}}=erlang:localtime(),
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w",[YYYY, MM, DD])).

string_now()->
    {{YYYY,Mm,DD},{HH,MM,SS}}=erlang:localtime(),
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[YYYY, Mm, DD, HH, MM, SS])).
    
string_now_micro()->
    {{YYYY,Mm,DD},{HH,MM,SS}} = erlang:localtime(),
    {_MegaSecs, _Secs, MicroSecs} = os:timestamp(),
    lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w~3..0w",[YYYY, Mm, DD, HH, MM, SS, MicroSecs])).

string_tick()->
    integer_to_list(long_tick()).

long_tick()->
    calendar:datetime_to_gregorian_seconds(erlang:localtime()).

time_diff(T1,T2)->
    {Days, {HH, MM, SS}} = calendar:time_difference(T1, T2),
    Days*24*3600+HH*3600+MM*60+SS.