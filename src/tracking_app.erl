%%%-------------------------------------------------------------------
%% @doc tracking public API
%% @end
%%%-------------------------------------------------------------------

-module(tracking_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_h, []},
            {"/query_facility", query_facility_h, [qf_dist]},
            {"/query_package_history", query_package_info_h, [qp_dist]},
            {"/query_vehicle_history", query_vehicle_history_h,[qv_dist]},
            {"/store_facility_info", store_facility_info_h, [sf_dist]},
            {"/store_package_info", store_package_info_h, [sp_dist]},
            {"/store_vehicle_info", store_vehicle_info_h, [sv_dist]}
        ]} 
    ]),
    PrivDir = code:priv_dir(tracking),
    {ok,_} = cowboy:start_tls(https_listener, [
            {port, 443},
            {certfile, PrivDir ++ "/ssl/fullchain.pem"},
            {keyfile, PrivDir ++ "/ssl/privkey.pem"}
        ], #{env => #{dispatch => Dispatch}}),
    
    tracking_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
