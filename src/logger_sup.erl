%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%	日志管理者
%%% @end
%%% Created : 15. 八月 2019 22:21
%%%-------------------------------------------------------------------
-module(logger_sup).

%%%=======================STATEMENT====================
-copyright('').
-author("WeiMengHuan, SuperMuscleMan@outlook.com").
-vsn(1).

%%%=======================EXPORT=======================
-export([]).
%%%=======================INCLUDE======================

%%%=======================RECORD=======================
%%%
%%%=======================DEFINE=======================

%%%=================EXPORTED FUNCTIONS=================

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(CfgDir :: string()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CfgDir) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [CfgDir]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
		MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
		[ChildSpec :: supervisor:child_spec()]
	}} |
	ignore |
	{error, Reason :: term()}).
init([CfgDir]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	
	ChildList = child_spec(CfgDir),
%%	AChild = {'AName', {'AModule', start_link, []},
%%		Restart, Shutdown, Type, ['AModule']},
	
	{ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% -----------------------------------------------------------------
%% Func:
%% Description:子进程配置
%% Returns:
%% -----------------------------------------------------------------
child_spec(CfgDir) ->
	NewConfig = validate_cfg(CfgDir),
	do_child_spec(NewConfig, []).
do_child_spec([{LogName, Cfg} | T], Result) ->
	
	Restart = permanent,
	Shutdown = 2000,
	Type = worker,
	
	Child = {LogName, {logger, start_link, [LogName, Cfg]},
		Restart, Shutdown, Type, [logger]},
	do_child_spec(T, [Child | Result]);
do_child_spec([], Result) ->
	Result.

%% -----------------------------------------------------------------
%% Func:
%% Description:验证配置
%% Returns:
%% -----------------------------------------------------------------
validate_cfg(CfgDir) ->
	{ok, Cfg} = file:consult(CfgDir),
	case proplists:get_value(log_child_spec, Cfg) of
		undefined ->
			{error, "cfg_undefined"};
		SpecCfg ->
			SpecCfg
	end.
