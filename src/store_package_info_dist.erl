-module(store_package_info_dist).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/1,start_link/1,stop/1,call/0]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).


%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(term()) -> {ok, atom()}.
start(Initial_state) ->
    gen_statem:start({local,?MODULE}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(term()) -> {ok, atom()}.
start_link(Initial_state) ->
    gen_statem:start_link({local,?MODULE},?MODULE,Initial_state,Initial_state).


%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the balancer.
%%
%% The parameter of stop is an atom that
%% is a registered name of a round robin balancer.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private
init(Worker_ids) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok,ready,Worker_ids}.
%% @private
callback_mode() -> handle_event_function.

%%% state callback(s)
call() ->
    gen_statem:call(distributor,next).

%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
handle_event({call,From}, next, ready,[H|T]) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, ready,lists:append(T,[H]),[{reply,From,H}]}.


%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
start_test() ->
    {setup,
        fun() -> 2 end,
        fun() -> gen_server:stop(?SERVER), gen_server:stop(testing_name) end,
        [?_assertEqual(ok, start()), ?_assertEqual(ok,start(local,testing_name))]}.

stop_test() ->
    {setup,
        fun() -> gen_server:start_link({local, add}, ?MODULE, [], []) end,
        fun() -> ok end,
        [?_assertEqual(ok, stop())]}.

handle_events_() ->
    [?_assertMatch(10, add([2,3,4,1])),
    ?_assertMatch(7, add([7])),
    ?_assertMatch(7, add(7)),
    ?_assertMatch(nil, add([]))].


-endif.