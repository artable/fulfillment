%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a template for supervisors that start up a defined set of 
%%% OTP behaviors.
%%%
%%%
%%% @end

%%% Created : 24 October 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(distributor_sup).
-behaviour(supervisor).

%%%===================================================================
%%% Make sure to complete the documentation to match
%%% your supervisor's behavior and requirements.
%%%===================================================================

%% 
-export([init/1]).
%% event Callbacks
-export([start/1,start/3]).

%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there is only one such supervisor created
%% and it is registered locally under the module name.
%%
%%
%% @end
%%--------------------------------------------------------------------
start(Start_info)->
    supervisor:start_link({local,?MODULE},?MODULE,Start_info).

%%--------------------------------------------------------------------
%% @doc
%%
%% Used if there can be many supervisors of this type
%% or if the supervisor is to be registered in any way 
%% but locally.
%%
%%
%% @end
%%--------------------------------------------------------------------
start(Supervisor_name,Registration_type,Start_info)->
    supervisor:start_link({Registration_type,Supervisor_name},?MODULE,Start_info).


%%%===================================================================
%%% Mandatory callback functions
%%%===================================================================

init({Distributor_id, Worker_ids, Worker_module}) ->

%% A supervisor specification is a record with the following mappings.
%%
%% supervisor_spec() = #{strategy()=>atom(),    % mandatory. Options are: 1)one_for_one, 2)one_for_all, 3)rest_for_one, and 4)simple_one_for_one.
%%                       intensity => integer(),% mandatory. Number of restarts allowed in period.
%%                       period => integer()}   % mandatory. A number of seconds.
    
    %%
    %% define your supervisor specification here.
    %%

    %%
    %% generate your child specification list here.
    %%

    %%
    %% This function has a value that is a tuple
    %% consisting of ok and a tuple that is the supervisor specifiation list 
    %% followed by the list of child specifications.
    %%
    Sup_spec = #{
        strategy => one_for_all,
        intensity => 1000,
        period => 1
    },
    Worker_specs = [distributor_spec(Distributor_id) |
                    worker_specs(Worker_ids, Worker_module)],
    {ok,{Sup_spec, Worker_specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
distributor_spec(Id) ->
    generate_worker_spec(Id, distributor).

worker_specs(Ids, Module)->
    Gen = fun (Id) -> generate_worker_spec(Id, Module) end,
    lists:map(Gen, Ids).

generate_worker_spec(Name, Module) ->
    #{id => Name,
    start => {Module,start_link,[Name]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [Module]}.

%%@private
generate_spec(Module,Type)->
%%
%% A child Specification is a record with the following mappings.
%%
%% child_spec() = #{id => child_id(),       % mandatory. The name to be registered.
%%                  start => mfargs(),      % mandatory. The module's startup function.
%%                  restart => atom(),              % optional. Options are permanent (restart always), transient (restart only after abnormal termination), and temporary (never restart).
%%                  shutdown => integer()|atom(),   % optional. A number or the atom infinity representing the milliseconds allowed for a soft, normal shutdown before it is killed brutally.
%%                  type => atom(),                 % optional. Options are worker or supervisor.
%%                  modules => [module()]|atom()}   % optional. A list of modules to be considered for upgrading
%%                                                  % when the child's code is upgraded. The dynamic atom is used for when 
%%                                                  % such a list is unknown, for example when the child is a 
%%                                                  % gen_event manager with some unknown types of gen_event handler
%%                                                  % modules to be added later.  
        #{id => Module,
          start => {Module,start_link,[]},
          restart => permanent,
          shutdown => 2000,
          type => Type,
          modules => [Module]}.
