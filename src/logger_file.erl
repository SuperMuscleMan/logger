-module(logger_file).
%%%=======================STATEMENT====================
-description("logger_file").
-copyright('').
-author("wmh, weimenghuan@qixia.com").
-vsn(1).
%%%=======================EXPORT=======================
-export([init/3, write/3, log/6]).
%%%=======================INCLUDE======================

%%%=======================RECORD=======================

%%%=======================DEFINE=======================
-define(File_Name_Suffix, ".log").
%%%=================EXPORTED FUNCTIONS=================

%% -----------------------------------------------------------------
%% Func: 
%% Description: 日志文件初始化
%% Returns: 
%% -----------------------------------------------------------------
init(FileDir, Interval, NumLimit) ->
	case filelib:ensure_dir(FileDir) of
		ok ->
			create_file(FileDir, logger_lib:local_second(), Interval, NumLimit);
		Error ->
			erlang:error({"make_dir_fail", Error, {fileDir, FileDir}}, [FileDir, Interval])
	end.
%% -----------------------------------------------------------------
%% Func:
%% Description:创建文件
%% Returns:
%% -----------------------------------------------------------------
create_file(FileDir, LocalSecond, Interval, NumLimit) ->
	Dir = filename:dirname(FileDir),
	BaseName = filename:basename(FileDir),
	FileName = make_time_name(LocalSecond, BaseName),
	case file_io_open(Dir, FileName) of
		{ok, IoDevice} ->
			NextSecond = logger_lib:get_cycle_second(LocalSecond, Interval),
			del_file_out(Dir, BaseName, NumLimit),
			{IoDevice, NextSecond};
		Error ->
			erlang:error({"file_open_fail", Error, {filename, FileName}})
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description:记录日志
%% Returns:
%% -----------------------------------------------------------------
log(IoDevice, FileDir, NextSecond, Interval, NumLimit, LogList) ->
	LocalSecond = logger_lib:local_second(),
	{NewIoDevice, NewNextSecond} =
		if
			LocalSecond < NextSecond ->
				{IoDevice, NextSecond};
			true ->
				case file:close(IoDevice) of
					ok ->
						create_file(FileDir, logger_lib:get_cycle_second(LocalSecond - Interval, Interval), Interval, NumLimit);
					Error ->
						erlang:error({"file_close_fail", Error, {filename, FileDir}})
				end
		end,
	write(NewIoDevice, FileDir, LogList),
	{NewIoDevice, NewNextSecond}.

%% -----------------------------------------------------------------
%% Func:
%% Description:写入文件
%% Returns:
%% -----------------------------------------------------------------
write(_, _, []) ->
	ok;
write(IoDevice, FileDir, [Msg | T]) ->
	case file:write(IoDevice, format(Msg)) of
		ok ->
			write(IoDevice, FileDir, T);
		Error ->
			erlang:error({"wrtie_log_fail", Error}, [IoDevice, FileDir, Msg])
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description:删除过期的
%% Returns:
%% -----------------------------------------------------------------
del_file_out(Dir, BaseName, Limit) ->
	Prefix = filename:rootname(BaseName),
	{ok, FileList} = file:list_dir(Dir),
	ValiList = [E || E <- FileList, string:str(E, Prefix) > 0],
	Len = length(ValiList),
	if
		Len < Limit ->
			ok;
		true ->
			{DelList, _} = lists:split(Len - Limit, ValiList),
			del_file_out_(Dir, DelList)
	end.

del_file_out_(_Dir, []) ->
	ok;
del_file_out_(Dir, [FileName | T]) ->
	catch file:delete(filename:join(Dir, FileName)),
	del_file_out_(Dir, T).

%% -----------------------------------------------------------------
%% Func:
%% Description:格式化log格式
%% Returns:
%% -----------------------------------------------------------------
format({log, MilliSecond, Pid, Mod, Line, Data}) ->
	{{Y, M, D}, {H, Minute, S}, MilliSec} = logger_lib:milli_second_to_datetime(MilliSecond),
	[
		logger_lib:integer_to_list(Y, 4), $-,
		logger_lib:integer_to_list(M, 2), $-,
		logger_lib:integer_to_list(D, 2), $\s,
		logger_lib:integer_to_list(H, 2), $:,
		logger_lib:integer_to_list(Minute, 2), $:,
		logger_lib:integer_to_list(S, 2), $.,
		logger_lib:integer_to_list(MilliSec, 3), $\s,
		pid_to_list(Pid), $\s,
		atom_to_list(Mod), $:,
		integer_to_list(Line), $\s,
		logger_lib:term_to_string(Data),
		$\n
	].

%% -----------------------------------------------------------------
%% Func:
%% Description:创建文件时间名
%% Returns:
%% -----------------------------------------------------------------
make_time_name(Second, BaseName) ->
	Prefix = filename:rootname(BaseName),
	Suffix = filename:extension(BaseName),
	{{Year, Month, Day}, {Hour, Minute, _}} = logger_lib:second_to_datetime(Second),
	[Prefix,
		logger_lib:integer_to_list(Year, 4), $-,
		logger_lib:integer_to_list(Month, 2), $-,
		logger_lib:integer_to_list(Day, 2), $_,
		logger_lib:integer_to_list(Hour, 2), $-,
		logger_lib:integer_to_list(Minute, 2),
		Suffix].

%% -----------------------------------------------------------------
%% Func:
%% Description: 开启文件io句柄
%% Returns:
%% -----------------------------------------------------------------
file_io_open(Dir, FileName) ->
	FileDir = filename:join(Dir, FileName),
	case file:open(FileDir, [append, raw]) of
		{ok, IoDevice} ->
			{ok, IoDevice};
		Error ->
			{error, Error}
	end.


%==========================DEFINE=======================