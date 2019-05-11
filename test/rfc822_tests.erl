-module(rfc822_tests).

-include_lib("eunit/include/eunit.hrl").

from_datetime1_test_() ->
    Cases = [
        {
            "2019-05-02T00:00:00Z",
            {{2019,5,2},{0,0,0}},
            "Thu, 02 May 2019 00:00:00 +0000"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = rfc822:from_datetime(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

from_datetime2_test_() ->
    Cases = [
        {
            "2019-05-02 00:00:00 GMT",
            {{{2019,5,2},{0,0,0}}, "GMT"},
            "Thu, 02 May 2019 00:00:00 GMT"
        },
        {
            "2019-05-02 00:00:00 +0000",
            {{{2019,5,2},{0,0,0}}, "+0000"},
            "Thu, 02 May 2019 00:00:00 +0000"
        },
        {
            "2019-05-02 00:00:00 -0000",
            {{{2019,5,2},{0,0,0}}, "-0000"},
            "Thu, 02 May 2019 00:00:00 +0000"
        },
        {
            "2019-05-02 09:30:00 +0930",
            {{{2019,5,2},{0,0,0}}, "+0930"},
            "Thu, 02 May 2019 09:30:00 +0930"
        },
        {
            "2019-05-01 20:30:00 -0330",
            {{{2019,5,2},{0,0,0}}, "-0330"},
            "Wed, 01 May 2019 20:30:00 -0330"
        }
    ],
    F = fun({Name, {Input, Z}, Expected}) ->
        Actual = rfc822:from_datetime(Input, Z),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

from_system_time1_test_() ->
    Cases = [
        {
            "2019-05-02T07:47:31Z",
            1556783251,
            "Thu, 02 May 2019 07:47:31 +0000"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = rfc822:from_system_time(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

from_system_time2_test_() ->
    Cases = [
        {
            "2019-05-02 07:47:31 GMT",
            {1556783251, "GMT"},
            "Thu, 02 May 2019 07:47:31 GMT"
        },
        {
            "2019-05-02 07:47:31 +0000",
            {1556783251, "+0000"},
            "Thu, 02 May 2019 07:47:31 +0000"
        },
        {
            "2019-05-02 07:47:31 -0000",
            {1556783251, "-0000"},
            "Thu, 02 May 2019 07:47:31 +0000"
        },
        {
            "2019-05-02 17:17:31 +0930",
            {1556783251, "+0930"},
            "Thu, 02 May 2019 17:17:31 +0930"
        },
        {
            "2019-05-02 04:17:31 -0330",
            {1556783251, "-0330"},
            "Thu, 02 May 2019 04:17:31 -0330"
        }
    ],
    F = fun({Name, {Input, Z}, Expected}) ->
        Actual = rfc822:from_system_time(Input, Z),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

from_timestamp1_test_() ->
    Cases = [
        {
            "2019-05-02T09:53:17Z",
            {1556,790797,212780},
            "Thu, 02 May 2019 09:53:17 +0000"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = rfc822:from_timestamp(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

from_timestamp2_test_() ->
    Cases = [
        {
            "2019-05-02 09:53:17 GMT",
            {{1556,790797,212780}, "GMT"},
            "Thu, 02 May 2019 09:53:17 GMT"
        },
        {
            "2019-05-02 09:53:17 +0000",
            {{1556,790797,212780}, "+0000"},
            "Thu, 02 May 2019 09:53:17 +0000"
        },
        {
            "2019-05-02 09:53:17 -0000",
            {{1556,790797,212780}, "-0000"},
            "Thu, 02 May 2019 09:53:17 +0000"
        },
        {
            "2019-05-02 18:23:17 +0930",
            {{1556,790797,212780}, "+0930"},
            "Thu, 02 May 2019 19:23:17 +0930"
        },
        {
            "2019-05-02 06:23:17 -0330",
            {{1556,790797,212780}, "-0330"},
            "Thu, 02 May 2019 06:23:17 -0330"
        }
    ],
    F = fun({Name, {Input, Z}, Expected}) ->
        Actual = rfc822:from_timestamp(Input, Z),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

to_datetime_test_() ->
    Cases = [
        {
            "Thu, 02 May 2019 00:00:00 GMT",
            {{2019,5,2},{0,0,0}}
        },
        {
            "Thu, 02 May 2019 00:00:00 +0000",
            {{2019,5,2},{0,0,0}}
        },
        {
            "Thu, 02 May 2019 09:30:00 +0930",
            {{2019,5,2},{0,0,0}}
        },
        {
            "Wed, 01 May 2019 20:30:00 -0330",
            {{2019,5,2},{0,0,0}}
        }
    ],
    F = fun({Input, Expected}) ->
        Actual = rfc822:to_datetime(Input),
        {Input, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

to_system_test_() ->
    Cases = [
        {
            "Thu, 02 May 2019 00:00:00 GMT",
            1556755200
        },
        {
            "Thu, 02 May 2019 00:00:00 +0000",
            1556755200
        },
        {
            "Thu, 02 May 2019 09:30:00 +0930",
            1556755200
        },
        {
            "Wed, 01 May 2019 20:30:00 -0330",
            1556755200
        }
    ],
    F = fun({Input, Expected}) ->
        Actual = rfc822:to_system_time(Input),
        {Input, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

to_timestamp_test_() ->
    Cases = [
        {
            "Thu, 02 May 2019 09:53:17 GMT",
            {1556,790797,0}
        },
        {
            "Thu, 02 May 2019 09:53:17 +0000",
            {1556,790797,0}
        },
        {
            "Thu, 02 May 2019 19:23:17 +0930",
            {1556,790797,0}
        },
        {
            "Thu, 02 May 2019 06:23:17 -0330",
            {1556,790797,0}
        }
    ],
    F = fun({Input, Expected}) ->
        Actual = rfc822:to_timestamp(Input),
        {Input, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
