%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_plan).

-export([id/1
        ,set_id/2
        ]).
-export([vendor_id/1
        ,set_vendor_id/2
        ]).
-export([bookkeeper_id/1
        ,set_bookkeeper_id/2
        ]).
-export([jobj/1
        ,set_jobj/2
        ]).
-export([plan_jobj/1
        ,set_plan_jobj/2
        ]).
-export([overrides/1
        ,set_overrides/2
        ]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).

-export([fetch/2]).
-export([create/3]).
-export([assign/2
        ,assign/3
        ]).
-export([unassign/2]).
-export([override/3
        ,override/4
        ]).

-record(plan, {id :: kz_term:ne_binary()
              ,vendor_id :: kz_term:ne_binary()
              ,bookkeeper_id :: kz_term:ne_binary()
              ,jobj = kz_json:new() :: kz_json:object()
              ,plan_jobj = kz_json:new() :: kz_json:object()
              ,overrides = kz_json:new() :: kz_json:object()
              }
       ).

-opaque plan() :: #plan{}.
-type setter_fun() :: {fun((plan(), Value) -> plan()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([plan/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(plan()) -> kz_term:ne_binary().
id(#plan{id=Id}) ->
    Id.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_id(plan(), kz_term:ne_binary()) -> plan().
set_id(Plan, Id) ->
    Plan#plan{id=Id}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec vendor_id(plan()) -> kz_term:ne_binary().
vendor_id(#plan{vendor_id=VendorId}) ->
    VendorId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_vendor_id(plan(), kz_term:ne_binary()) -> plan().
set_vendor_id(Plan, VendorId) ->
    Plan#plan{vendor_id=VendorId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_id(plan()) -> kz_term:ne_binary().
bookkeeper_id(#plan{bookkeeper_id=BookkeeperId}) ->
    BookkeeperId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_id(plan(), kz_term:ne_binary()) -> plan().
set_bookkeeper_id(Plan, BookkeeperId) ->
    Plan#plan{bookkeeper_id=BookkeeperId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec jobj(plan()) -> kz_json:object().
jobj(#plan{jobj=JObj}) ->
    JObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_jobj(plan(), kz_json:object()) -> plan().
set_jobj(Plan, JObj) ->
    Plan#plan{jobj=JObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan_jobj(plan()) -> kz_term:ne_binary().
plan_jobj(#plan{plan_jobj=PlanJObj}) ->
    PlanJObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan_jobj(plan(), kz_term:ne_binary()) -> plan().
set_plan_jobj(Plan, PlanJObj) ->
    Plan#plan{plan_jobj=PlanJObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec overrides(plan()) -> kz_json:object().
overrides(#plan{overrides=Overrides}) ->
    Overrides.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_overrides(plan(), kz_json:object()) -> plan().
set_overrides(Plan, Overrides) ->
    case kz_term:is_empty(Overrides) of
        'true' -> Plan;
        'false' ->
            JObj = kz_json:merge_recursive(plan_jobj(Plan), Overrides),
            Setters = [{fun set_bookkeeper_id/2, plan_bookkeeper_id(JObj)}
                      ,{fun set_jobj/2, JObj}
                      ],
            setters(Plan, Setters)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> plan().
empty() ->
    #plan{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> plan().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(plan(), setter_funs()) -> plan().
setters(Plan, Routines) ->
    lists:foldl(fun({Setter, Value}, P) ->
                        Setter(P, Value)
                end
               ,Plan
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(plan()) -> kz_json:object().
public_json(Plan) ->
    kz_doc:public_fields(plan_jobj(Plan)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_term:ne_binary()) -> 'undefined' | plan().
fetch(VendorId, PlanId) ->
    lager:debug("fetching plan ~s/~s", [VendorId, PlanId]),
    VendorDb = kz_util:format_account_db(VendorId),
    case kz_datamgr:open_cache_doc(VendorDb, PlanId) of
        {'ok', PlanJObj} ->
            create(VendorId, PlanId, PlanJObj);
        {'error', _R} ->
            lager:info("unable to open service plan ~s/~s: ~p"
                      ,[VendorId, PlanId, _R]
                      ),
            'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> plan().
create(VendorId, PlanId, PlanJObj) ->
    Setters = [{fun set_id/2, PlanId}
              ,{fun set_vendor_id/2, VendorId}
              ,{fun set_bookkeeper_id/2, plan_bookkeeper_id(PlanJObj)}
              ,{fun set_jobj/2, PlanJObj}
              ,{fun set_plan_jobj/2, PlanJObj}
              ],
    setters(Setters).

-spec plan_bookkeeper_id(kz_json:object()) -> kz_term:ne_binary().
plan_bookkeeper_id(PlanJObj) ->
    kzd_service_plan:bookkeeper_id(PlanJObj, <<"default">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign(kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
assign(Services, PlanId) ->
    assign(Services, PlanId, []).

-spec assign(kz_services:services(), kz_term:ne_binary(), kz_term:proplist()) -> kz_services:services().
assign(Services, PlanId, Options) when is_binary(PlanId) ->
    ResellerId = kz_services_reseller:get_id(Services),
    Plan = kz_json:from_list(
             [{<<"vendor_id">>, ResellerId}
             ,{<<"overrides">>, kz_json:new()}
             ]),
    assign(Services, PlanId, Plan, Options);
assign(Services, JObj, Options) ->
    ResellerId = kz_services_reseller:get_id(Services),
    VendorId = kz_json:get_ne_binary_value(<<"vendor_id">>, JObj, ResellerId),
    Overrides = kz_json:get_json_value(<<"overrides">>, JObj, kz_json:new()),
    case kz_json:get_ne_value(<<"id">>, JObj) of
        'undefined' -> Services;
        PlanId ->
            Props = [{<<"vendor_id">>, VendorId}
                    ,{<<"overrides">>, Overrides}
                    ],
            Plan = kz_json:from_list(props:filter_undefined(Props)),
            assign(Services, PlanId, Plan, Options)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign(kz_services:services(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> kz_services:services().
assign(Services, PlanId, Plan, Options) ->
    ServicesJObj = kz_services:services_jobj(Services),
    lager:debug("adding service plan ~s/~s"
               ,[kzd_services:plan_vendor_id(ServicesJObj, PlanId)
                ,PlanId
                ]
               ),
    Overrides = kz_json:get_json_value(<<"overrides">>, Plan, kz_json:new()),
    Overriden = maybe_override(ServicesJObj, PlanId, Overrides, Options),
    UpdatedServicesJObj =
        kzd_services:set_plan(ServicesJObj
                             ,PlanId
                             ,kz_json:set_value(<<"overrides">>, Overriden, Plan)
                             ),
    kz_services:set_services_jobj(Services, UpdatedServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unassign(kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
unassign(Services, PlanId) ->
    lager:debug("removing service plan ~s", [PlanId]),
    ServicesJObj = kz_services:services_jobj(Services),
    kz_services:set_services_jobj(Services
                                 ,kzd_services:set_plan(ServicesJObj, PlanId, 'undefined')
                                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec override(kz_services:services(), kz_term:ne_binary(), kz_json:object()) -> kz_services:services().
override(Services, PlanId, Overrides) ->
    override(Services, PlanId, Overrides, []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec override(kz_services:services(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> kz_services:services().
override(Services, PlanId, Overrides, Options) ->
    ServicesJObj = kz_services:services_jobj(Services),
    Overriden = maybe_override(ServicesJObj, PlanId, Overrides, Options),
    kz_services:set_services_jobj(Services
                                 ,kzd_services:set_plan_overrides(ServicesJObj, PlanId, Overriden)
                                 ).

-spec maybe_override(kz_json:object(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> kz_json:object().
maybe_override(ServicesJObj, PlanId, Overrides, Options) ->
    case kzd_services:plan(ServicesJObj, PlanId) =/= 'undefined' of
        'false' -> kzd_services:plan_overrides(ServicesJObj, PlanId, kz_json:new());
        'true' -> set_or_merge_override(ServicesJObj, PlanId, Overrides, Options)
    end.

-spec set_or_merge_override(kz_json:object(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> kz_json:object().
set_or_merge_override(ServicesJObj, PlanId, Override, Options) ->
    case props:get_is_true('merge', Options, 'false') of
        'true' -> merge_override(ServicesJObj, PlanId, Override);
        'false' -> set_override(ServicesJObj, PlanId, Override)
    end.

-spec set_override(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
set_override(_ServicesJObj, PlanId, Overrides)  ->
    lager:debug("updating overrides for ~s via set", [PlanId]),
    Overrides.

-spec merge_override(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
merge_override(ServicesJObj, PlanId, Override) ->
    lager:debug("updating overrides for ~s via merge", [PlanId]),
    Overrides = kzd_services:plan_overrides(ServicesJObj, PlanId, kz_json:new()),
    kz_json:merge_recursive(Overrides, Override).