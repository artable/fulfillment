-module(query_package_history).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(SERVER, ?MODULE).

%% API
-export([start/0,start/1,start/3,stop/0,get_history/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.
get_history(UUID, PID) -> gen_server:call(PID, {get, UUID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
    case riakc_pb_socket:start_link("143.198.108.90", 8087) of 
        {ok,Riak_Pid} -> {ok,Riak_Pid};
        _ -> {stop,link_failure}
   end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
handle_call({get, UUID}, _From, Riak_Pid) ->
    case  riakc_pb_socket:get(Riak_Pid, <<"package">>, UUID) of
        {ok,Fetched} -> 
            {reply,binary_to_list(riakc_obj:get_value(Fetched)),Riak_Pid};
        Error -> {reply,Error,Riak_Pid}
    end;
handle_call(stop, _From, _State) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================



-ifdef(EUNIT).
%%
handle_call_test() -> 
    {setup,
    fun()-> 
        meck:new(riakc_obj),
        meck:new(riakc_pb_socket),
        meck:expect(riakc_obj, get_value, fun(_Bucket,_Key,_Value) -> done end),
        meck:expect(riakc_pb_socket, get, fun(_Riak_pid,_Request, _Name) -> 
            
            case Request of
                {<<"123">>} -> [#{}]
                _ -> {error, novalue}
            end
                end)
    end,
    fun()-> 
        meck:unload(riakc_obj),
        meck:unload(riakc_pb_socket)
    end,
    [?_assert_match(query_facility:handle_call({query_package_hist,<<"123">>}, some_from_pid, some_riak_pid), {reply,[#{"holder_uuid" =: "h_1234"}, #{"timestamp" =: "12345"},],some_riak_pid}),
     ?_assert(query_facility:handle_call({query_package_hist,<<"1234">>,<<"fac">>}, some_from_pid, some_riak_pid) =:= {reply,<<"Seattle">>,some_riak_pid}),
     ?_assert(query_facility:handle_call({query_fac,<<"">>,<<"fac">>}, some_from_pid, some_riak_pid) =:= {reply,{fail,empty_key},some_riak_pid}),
     ?_assert(query_facility:handle_call({query_fac,<<"taco">>,<<"fac">>}, some_from_pid, some_riak_pid) =:= {reply,{fail,not_found},some_from_pid})
    ]
}.
%%
-endif.