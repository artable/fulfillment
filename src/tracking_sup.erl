%%%-------------------------------------------------------------------
%% @doc tracking top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tracking_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    Sup_specs = fun ({Section, N_workers, W_module}) ->
        Sup_name = atom_to_list(Section) ++ "_sup",
                   child_sup(list_to_atom(Sup_name), 
                  {Section, N_workers, W_module})
    end,
    ChildSpecs = lists:map(Sup_specs, distributor_config()),


    {ok, {SupFlags, ChildSpecs}}.


child_sup(Id, Start_info) ->
    
    #{id => Id,
    start => {distributor_sup,start,[Id, Start_info]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [distributor_sup]}.

%% internal functions
% qf_dist
% qp_dist
% qv_dist
% sf_dist
% sp_dist
% sv_dist
distributor_config() ->
    [
        {qf, 1, query_facility},
        {qp, 1, query_package_history},
        {qv, 1, query_vehicle_history},
        {sf, 1, store_facility_info},
        {sp, 1, store_package_info},
        {sv, 5, store_vehicle_info}
    ].