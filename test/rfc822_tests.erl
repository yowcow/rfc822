-module(rfc822_tests).

-include_lib("eunit/include/eunit.hrl").

from_datetime_test_() ->
    Cases = [
        {
            "2019-05-02T00:00:00Z",
            {{2019,5,2},{0,0,0}},
            "Thu, 02 May 2019 00:00:00 GMT"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = rfc822:from_datetime(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

from_now_test_() ->
    Cases = [
        {
            "2019-05-02T09:53:17Z",
            {1556,790797,212780},
            "Thu, 02 May 2019 09:53:17 GMT"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = rfc822:from_now(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

from_system_time_test_() ->
    Cases = [
        {
            "2019-05-02T07:47:31Z",
            1556783251,
            "Thu, 02 May 2019 07:47:31 GMT"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = rfc822:from_system_time(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
