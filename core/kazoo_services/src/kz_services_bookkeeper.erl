%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_bookkeeper).

-export([quick_sale/3]).
-export([maybe_update/1]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec quick_sale(kz_term:ne_binary(), integer(), kz_term:api_binary()) -> {'ok', kz_json:object()}.
quick_sale(_AccountId, _Amount, _Reason) ->
    {'ok', kz_json:new()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update(kz_services:services()) -> kz_services:services().
maybe_update(Services) ->
    case kz_services_invoices:has_changes(Services) of
        'false' ->
            lager:debug("no changes to any invoices", []),
            Services;
        'true' ->
            Invoices = kz_services_invoices:changed(Services),
            _BookkeeperResults =
                kz_services_invoices:foldl(invoices_foldl_fun(Services)
                                          ,[]
                                          ,Invoices
                                          ),
            Services
    end.

-spec invoices_foldl_fun(kz_services:services()) -> kz_services:invoices_foldl().
invoices_foldl_fun(Services) ->
    fun(Invoice, Results) ->
            Request = [{<<"Account-ID">>, kz_services:account_id(Services)}
                      ,{<<"Invoice">>, kz_services_invoice:public_json(Invoice)}
                      ,{<<"Audit">>, kz_services:audit_log(Services)}
                       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ],
            file:write_file("/tmp/test.json", io_lib:format("~s~n", [kz_json:encode(kz_json:from_list(Request))])),
            _Result = kz_amqp_worker:call(Request
                                         ,fun kapi_bookkeepers:publish_update_req/1
                                         ,fun kapi_bookkeepers:update_resp_v/1
                                         ),
            io:format("result: ~p~n", [_Result]),
            Results
    end.
