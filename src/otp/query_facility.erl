-module(query_facility).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(SERVER, ?MODULE).

%% API
-export([start/0,start/3,stop/0,get_city_of/1]).

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
get_city_of(Fac_UUID)-> gen_server:call(?MODULE, {query_fac,Fac_UUID}).

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
        {ok,replace_up}.
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
handle_call({query_fac, Fac_UUID}, From, State) ->
        {reply,replace_started,State};
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
        meck:expect(riakc_pb_socket, get, fun(_Bucket,_Key,_Value) -> done end),
        meck:expect(riakc_obj, get_value, fun(_Riak_pid,_Request, Name) -> 
            case _Request of
                <<"taco">> -> {error,notfound};
                <<"123">> -> {ok, <<"Rexburg">>};
                <<"1234">> -> {ok, <<"Seattle">>}
            end
                end)
    end,
    fun()-> 
        meck:unload(riakc_obj),
        meck:unload(riakc_pb_socket)
    end,
    [?_assert(query_facility:handle_call({query_fac,<<"123">>}, some_from_pid, some_riak_pid) =:= {reply,#{"city" => "Rexburg"},some_riak_pid}),
     ?_assert(query_facility:handle_call({query_fac,<<"1234">>}, some_from_pid, some_riak_pid) =:= {reply,#{"city" => "Seattle"},some_riak_pid}),
     ?_assert(query_facility:handle_call({query_fac,<<"">>}, some_from_pid, some_riak_pid) =:= {reply,{fail,empty_key},some_riak_pid}),
     ?_assert(query_facility:handle_call({query_fac,<<"taco">>}, some_from_pid, some_riak_pid) =:= {reply,{fail,not_found},some_from_pid})
    ]
}.
%%
-endif.