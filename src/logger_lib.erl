-module(logger_lib).
%%%=======================STATEMENT====================
-description("logger_lib").
-copyright('').
-author("wmh, weimenghuan@qixia.com").
-vsn(1).
%%%=======================EXPORT=======================
-export([analyze_name/1, datetime_to_second/1, second_to_datetime/1,
	second/0, local_second/0, get_cycle_second/2, milli_second_to_datetime/1,
	term_to_string/1, local_milli_second/0]).
-compile(export_all).
%%%=======================INCLUDE======================

%%%=======================RECORD=======================

%%%=======================DEFINE=======================
%%正则匹配 例如：2019—8-16_23-02 结果：{match,["2019-8-16_23-02","2019","8","16","23","02"]}
-define(Rex_Time, "(\\d\+)-(\\d\+)-(\\d\+)_(\\d\+)-(\\d\+)").
%%一天的秒数
-define(Day_Seconds, 86400).
%%一天的分钟数
-define(Day_Minute, 1440).
%%1970年的天数
-define(Days_1970, 719528).
%%东八区秒数(3600*8)
-define(DongBaDistrct, 28800).
%%%=================EXPORTED FUNCTIONS=================

%% -----------------------------------------------------------------
%% Func:
%% Description: 获取下一个循环时间点(整点)
%% Returns:
%% -----------------------------------------------------------------
get_cycle_second(Second, Interval) ->
	Rest = Second rem ?Day_Seconds,
	(Second - Rest) + ((Rest div Interval + 1) * Interval).

%% -----------------------------------------------------------------
%% Func:
%% Description: 解析文件名中的时间
%% Returns:
%% -----------------------------------------------------------------
analyze_name(Name) ->
	case re:run(Name, ?Rex_Time, [{capture, all, list}]) of
		{match, [_TimeString, Year, Mouth, Day, Hour, Minute]} ->
			{{Year, Mouth, Day}, {Hour, Minute, 0}};
		_ ->
			{{0, 0, 0}, {0, 0, 0}}
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description:日期到秒（1970年的秒）
%% Returns:
%% -----------------------------------------------------------------
datetime_to_second({Data, Time}) ->
	Days = calendar:date_to_gregorian_days(Data) - ?Days_1970,
	calendar:time_to_seconds(Time) + Days * ?Day_Seconds.

%% -----------------------------------------------------------------
%% Func:
%% Description:秒到日期
%% Returns:
%% -----------------------------------------------------------------
second_to_datetime(Secs) ->
	Days = Secs div ?Day_Seconds,
	Rest = Secs rem ?Day_Seconds,
	{calendar:gregorian_days_to_date(Days + ?Days_1970), calendar:seconds_to_time(Rest)}.

%% -----------------------------------------------------------------
%% Func:
%% Description:毫秒到日期
%% Returns:
%% -----------------------------------------------------------------
milli_second_to_datetime({MegaSec, Sec, MilliSec}) ->
	{Date, Time} = second_to_datetime(MegaSec * 1000000 + Sec),
	{Date, Time, MilliSec div 1000}.

%% -----------------------------------------------------------------
%% Func:
%% Description:获取时间戳(世界时间)
%% Returns:
%% -----------------------------------------------------------------
second() ->
	{A, B, _C} = os:timestamp(),
	A * 1000000 + B.

%% -----------------------------------------------------------------
%% Func:
%% Description:获取本地时间戳(东八区时间)
%% Returns:
%% -----------------------------------------------------------------
local_second() ->
	second() + ?DongBaDistrct.

%% -----------------------------------------------------------------
%% Func:
%% Description:获取本地时间戳(毫秒级，东八区)
%% Returns:
%% -----------------------------------------------------------------
local_milli_second() ->
	{Mega, Secs, Milli} = os:timestamp(),
	Total = Mega * 1000000 + Secs + ?DongBaDistrct,
	{Total div 1000000, Total rem 1000000, Milli}.

%% -----------------------------------------------------------------
%% Func:
%% Description:erlang数据结构 转换为 String表达式
%% Returns:
%% -----------------------------------------------------------------
term_to_string(E) ->
	lists:reverse(term_to_string(E, [])).
term_to_string(E, R) when is_atom(E) ->
	[erlang:atom_to_list(E) | R];
term_to_string(E, R) when is_pid(E) ->
	[erlang:pid_to_list(E) | R];
term_to_string(E, R) when is_port(E) ->
	[erlang:port_to_list(E) | R];
term_to_string(E, R) when is_integer(E) ->
	[erlang:integer_to_list(E) | R];
term_to_string(E, R) when is_float(E) ->
	[erlang:float_to_list(E) | R];
term_to_string(E, R) when is_tuple(E) ->
	L = erlang:tuple_to_list(E),
	[$} | list_to_string(L, [${ | R])];
term_to_string(E, R) when is_binary(E) ->
	[$>, $> | list_to_string(erlang:binary_to_list(E), [$<, $< | R])];
term_to_string(E, R) when is_list(E) ->
	case [C || C <- E, C > 127 orelse C < 32] of
		[] when E == [] ->
			[$], $[ | R];
		[] ->
			[$" | str_to_string(E, [$" | R])];
		_ ->
			[$] | list_to_string(E, [$[ | R])]
	end;
term_to_string(E, R) ->
	Bin = erlang:term_to_binary(E),
	term_to_string(Bin, R).
%% 将[] 转换为string表达式
list_to_string([], R) ->
	R;
list_to_string([H], R) ->
	term_to_string(H, R);
list_to_string([H | T], R) ->
	R1 = term_to_string(H, R),
	list_to_string(T, [$, | R1]).
%% 将“” 转换为 string表达式
str_to_string([], R) ->
	R;
str_to_string([H | T], R) ->
	str_to_string(T, [H | R]).

%% -----------------------------------------------------------------
%% Func:
%% Description:整数转换字符串 指定占位个数
%% Returns:
%% -----------------------------------------------------------------
integer_to_list(Int, Count) when Int < 1 ->
	add_zero(integer_to_list(Int), Count - 1);
integer_to_list(Int, Count) ->
	Place = Count - (erlang:trunc(math:log10(Int)) + 1),
	add_zero(integer_to_list(Int), Place).
add_zero(Base, Count) when Count < 1 ->
	Base;
add_zero(Base, Count) ->
	add_zero([$0 | Base], Count - 1).
%==========================DEFINE=======================