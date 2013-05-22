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

-module(util_convert).
-author('zhengxujin@adsage.com').

-export([to_string/2, to_string/1, to_integer/1, to_atom/1]).

to_string(Num, Length) when is_integer(Num)->
            N = erlang:integer_to_list(Num),
            ensure_length(N,Length);
to_string(Num, _Length)->Num.


to_string(Num) when is_integer(Num)->
    erlang:integer_to_list(Num);
to_string(Num) when is_atom(Num)->
    erlang:atom_to_list(Num);
to_string(Num) when is_binary(Num)->
    erlang:binary_to_list(Num);
to_string(Num) when is_float(Num)->
    erlang:float_to_list(Num);
to_string(Num)->Num.

ensure_length(S,Length) when is_integer(Length),is_list(S)->
    string:right(S,Length,$0);
ensure_length(S,_Length)->S.


to_integer(null)->
    0;
to_integer([])->
    0;
to_integer(Num) when is_integer(Num)->
    Num;
to_integer(Num) when is_binary(Num)->
    to_integer(binary_to_list(Num));
to_integer(Num) when is_list(Num)->
    list_to_integer(Num).


to_atom(Val) when is_list(Val)->
    list_to_atom(Val);
to_atom(Val) when is_atom(Val)->
    Val.