%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_refund).

-export([handle_req/2]).

-include("braintree.hrl").

-define(DEFAULT_ERROR_MESSAGE, <<"unknown error">>).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_bookkeepers:refund_req_v(JObj),
    case kz_json:get_ne_binary_value(<<"Bookkeeper-Type">>, JObj) =:= <<"braintree">> of
        'false' -> 'ok';
        'true' -> validate_request(JObj)
    end.

-spec validate_request(kz_json:object()) -> 'ok'.
validate_request(JObj) ->
    _VendorId = kz_json:get_ne_binary_value(<<"Vendor-ID">>, JObj),
    AccountId = kz_json:get_ne_binary_value(<<"Account-ID">>, JObj),
    DollarAmount =
        kz_currency:units_to_dollars(
          kz_json:get_integer_value(<<"Amount">>, JObj, 0)
         ),
    Result = do_request(AccountId, DollarAmount),
    Resp =
        kz_json:from_list(
          [{?KEY_MSG_ID, kz_api:msg_id(JObj)}
          ,{<<"Transaction-ID">>, kz_json:get_ne_binary_value(<<"Transaction-ID">>, JObj)}
          ,{<<"Transaction-DB">>, kz_json:get_ne_binary_value(<<"Transaction-DB">>, JObj)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ] ++ Result
         ),
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_bookkeepers:publish_refund_resp(ServerId, P) end,
    kz_amqp_worker:cast(Resp, Publisher).

-spec do_request(kz_term:ne_binary(), kz_currency:dollars()) -> 'ok'.
do_request(AccountId, DollarAmount) ->
    try braintree_transaction:quick_credit(AccountId, DollarAmount) of
        Transaction ->
            [{<<"Status">>, <<"success">>}
            ,{<<"Details">>, braintree_transaction:record_to_json(Transaction)}
            ]
    catch
        'throw':{'no_payment_token'=Reason, Details} ->
            Key = kz_term:to_binary(Reason),
            Message = kz_json:get_ne_binary_value(Key, Details, ?DEFAULT_ERROR_MESSAGE),
            [{<<"Message">>, Message}
            ,{<<"Status">>, <<"error">>}
            ,{<<"Reason">>, Key}
            ,{<<"Details">>, Details}
            ];
        'throw':{'api_error'=Reason, Details} ->
            Key = kz_term:to_binary(Reason),
            Errors = kz_json:get_value([Key, <<"errors">>], Details, []),
            Message = kz_json:get_ne_binary_value([Key, <<"message">>]
                                                 ,Details
                                                 ,?DEFAULT_ERROR_MESSAGE
                                                 ),
            [{<<"Message">>, Message}
            ,{<<"Status">>, <<"error">>}
            ,{<<"Reason">>, Key}
            ,{<<"Details">>, Errors}
            ];
        'throw':{'min_amount'=Reason, Details} ->
            Key = kz_term:to_binary(Reason),
            Message = kz_json:get_ne_binary_value(Key, Details, ?DEFAULT_ERROR_MESSAGE),
            [{<<"Message">>, Message}
            ,{<<"Status">>, <<"error">>}
            ,{<<"Reason">>, Key}
            ,{<<"Details">>, Details}
            ];
        'throw':{'max_amount'=Reason, Details} ->
            Key = kz_term:to_binary(Reason),
            Message = kz_json:get_ne_binary_value(Key, Details, ?DEFAULT_ERROR_MESSAGE),
            [{<<"Message">>, Message}
            ,{<<"Status">>, <<"error">>}
            ,{<<"Reason">>, Key}
            ,{<<"Details">>, Details}
            ];
        'throw':{Reason, Details} ->
            Key = kz_term:to_binary(Reason),
            Message = kz_json:get_ne_binary_value(Key, Details, ?DEFAULT_ERROR_MESSAGE),
            [{<<"Message">>, Message}
            ,{<<"Status">>, <<"fatal">>}
            ,{<<"Reason">>, Key}
            ,{<<"Details">>, Details}
            ]
    end.
