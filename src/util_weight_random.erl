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

-module(util_weight_random).

-export([get_random_data/2,get_random_data/3]).

get_random_data(_GetCount,[])->{0, []};
get_random_data(GetCount,Datas) when is_list(Datas)->
    Length = length(Datas),
    case Length > GetCount of 
        true->
            Indexs = generate_random_nums(GetCount, Length, []),
            Ret    = [lists:nth(Index,Datas)||Index<- Indexs],
            {length(Ret), Ret};
        false->
            {Length, Datas}
    end.

get_random_data(_GetCount, _WeightPos, [])-> {0, []};
get_random_data(GetCount, WeightPos, Datas) when is_list(Datas)->
    Data = get_random_data_1(GetCount, WeightPos, Datas, []),
    {length(Data), Data}.

get_random_data_1(0,      _WeightPos, _Datas, AccOut)-> lists:reverse(AccOut);
get_random_data_1(GetCount, WeightPos, Datas, AccOut) when is_list(Datas)->
    {WeightRanges, FDatas} = filter_and_summary_weight_list(WeightPos, Datas),
    case FDatas of 
         []->
            get_random_data_1(0, 0, [], AccOut);
         _->
            FLength = length(FDatas),
            case FLength =< GetCount of
                 true->
                    get_random_data_1(0, 0, [], FDatas);
                 false-> 
                    MaxNum    = hd(WeightRanges),
                    RandomNum = generate_random_num(MaxNum-1, []),
                    Index     = get_random_data_index2(1, RandomNum, WeightRanges),
                    Ret       = lists:nth(Index, FDatas),
                    NextData  = lists:subtract(FDatas, [Ret]),
                    get_random_data_1(GetCount-1, WeightPos, NextData, [Ret|AccOut])
            end
    end.    

filter_and_summary_weight_list(WeightPos, Datas)->
    lists:foldl(fun(Elem,AccIn  ={OutWeightRange = [H|_T], OutDatas})-> 
                         Weight = get_weight(WeightPos, Elem),
                         case Weight of 
                              0->
                                 AccIn;
                              _->
                                 {[Weight+H|OutWeightRange], [Elem|OutDatas]}
                         end
                 end,{[1],[]},Datas).

get_weight(WeightPos, Data) when is_list(Data)->
    lists:nth(WeightPos, Data);
get_weight(WeightPos, Data) when is_tuple(Data)->
    element(WeightPos, Data).

generate_random_nums(0,    _MaxNum, AccOut)->AccOut;
generate_random_nums(Count, MaxNum, AccOut)->
    Num = generate_random_num(MaxNum, AccOut),
    generate_random_nums(Count-1, MaxNum, [Num|AccOut]).

generate_random_num(0,     _Generated)->0;
generate_random_num(MaxNum, Generated)->
    Num = random:uniform(MaxNum),
    case lists:member(Num, Generated) of 
         true->
            generate_random_num(MaxNum, Generated);
         false->
            Num
    end.

get_random_data_index2(_Index, _RandomNum, [])->1;
get_random_data_index2(Index,   RandomNum, [WeightRange|T])->
    case T of 
         []->
              case RandomNum =< WeightRange of
                 true->
                    Index;
                 false->
                    get_random_data_index2(Index+1, RandomNum, T)
              end;
         [NextRange|_TT]->
             case (RandomNum < WeightRange) =:= (RandomNum >= NextRange) of
                 true->
                    Index;
                 false->
                    get_random_data_index2(Index+1, RandomNum, T)
             end
    end.