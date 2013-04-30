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

-module(util_config_reader).

-export([get_config_list/2,
         get_config_record/3,
         get_config_record/4]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("xmerl/include/xmerl_xpath.hrl").


get_config_record(FileName, XmlPath, Colums) 
                                when is_list(FileName),
                                is_list(XmlPath),
                                is_list(Colums)->
    XmlItems = get_config_list(FileName, XmlPath),
    convert_to_records(Colums, XmlItems, []).

get_config_record(FileName, XmlPath, RecordName, Colums) when is_atom(RecordName),is_list(FileName),is_list(XmlPath),is_list(Colums)->
    XmlItems = get_config_list(FileName, XmlPath, RecordName),
    convert_to_records(Colums, XmlItems, []).

get_config_list(FileName, XmlPath) when is_list(FileName),is_list(XmlPath)->
    get_config_list(FileName, XmlPath, undefined).
get_config_list(FileName, XmlPath, RecordName) when is_list(FileName),is_list(XmlPath)->
    {Doc, _Rest} = xmerl_scan:file(FileName),
    Mappings    = xmerl_xpath:string(XmlPath,Doc),
    lists:foldl(fun(MapItem,AccInElem)->
                        Attribs    = MapItem#xmlElement.attributes,
                        Name       = MapItem#xmlElement.name,
                        Out        = lists:foldl(fun(AttribItem, AccInAttrib)->
                                                    Value = AttribItem#xmlAttribute.value,
                                                    [{AttribItem#xmlAttribute.name, Value}|AccInAttrib]
                                              end,
                                              [],
                                              Attribs),
                        RName      = case RecordName of 
                                            undefined->
                                                Name;
                                           _->
                                                RecordName
                                      end,
                        WithRecord = [{elementName, RName}|Out],
                        [WithRecord|AccInElem]
                       end, [], Mappings).

convert_to_records(_Colums, [], Records)->
    Records;
convert_to_records(Colums, [XmlItem|T], Records)->
    Record = convert_to_record(Colums, XmlItem, []),
    convert_to_records(Colums, T, [Record|Records]).

convert_to_record([], XmlItem, ColumValues)->
    RecordName = proplists:get_value(elementName, XmlItem),
    list_to_tuple([RecordName|lists:reverse(ColumValues)]);
convert_to_record([{ColumName, ColumType}|T], XmlItem, ColumValues)->
    CurrentValue = proplists:get_value(ColumName, XmlItem),
    Value        = convert({CurrentValue,ColumType}),
    convert_to_record(T,XmlItem,[Value|ColumValues]).


convert({undefined,_})->
    undefined;
convert({Value,list}) when is_list(Value)->
    Value;
convert({Value,integer}) when is_list(Value)->
    list_to_integer(Value);
convert({Value,atom}) when is_list(Value)->
    list_to_atom(Value);
convert({Value,integer}) when is_integer(Value)->
    Value;
convert({Value,list}) when is_integer(Value)->
    integer_to_list(Value);
convert({Value,expr}) when is_list(Value)->
    eval_expression(Value,[]).

eval_expression(Exprs,Environ) ->
    {ok,Scanned,_} = erl_scan:string(Exprs),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value,V,_B}=erl_eval:exprs(Parsed,Environ),
    V.