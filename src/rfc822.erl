-module(rfc822).

-export([
    from_datetime/1,
    from_datetime/2,
    from_system_time/1,
    from_system_time/2,
    from_timestamp/1,
    from_timestamp/2,
    to_datetime/1,
    to_system_time/1,
    to_timestamp/1
]).

dow(1) -> "Mon";
dow(2) -> "Tue";
dow(3) -> "Wed";
dow(4) -> "Thu";
dow(5) -> "Fri";
dow(6) -> "Sat";
dow(7) -> "Sun".

moy(1)  -> "Jan";
moy(2)  -> "Feb";
moy(3)  -> "Mar";
moy(4)  -> "Apr";
moy(5)  -> "May";
moy(6)  -> "Jun";
moy(7)  -> "Jul";
moy(8)  -> "Aug";
moy(9)  -> "Sep";
moy(10) -> "Oct";
moy(11) -> "Nov";
moy(12) -> "Dec".

mon("Jan") -> 1;
mon("Feb") -> 2;
mon("Mar") -> 3;
mon("Apr") -> 4;
mon("May") -> 5;
mon("Jun") -> 6;
mon("Jul") -> 7;
mon("Aug") -> 8;
mon("Sep") -> 9;
mon("Oct") -> 10;
mon("Nov") -> 11;
mon("Dec") -> 12;
mon(_)     -> 0.

-define(DEFAULT_ZONE, "+0000").

format_rfc822({{Year, Mon, Day}, {H, M, S}}, Z) ->
    lists:flatten(io_lib:format("~3s, ~2.10.0B ~3s ~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ~s", [
        dow(calendar:day_of_the_week(Year, Mon, Day)),
        Day, moy(Mon), Year,
        H, M, S, Z
    ])).

-spec from_datetime(calendar:datetime()) -> string().
from_datetime(DateTime) ->
    from_datetime(DateTime, ?DEFAULT_ZONE).

-spec from_datetime(calendar:datetime(), string()) -> string().
from_datetime(DateTime, "GMT") ->
    format_rfc822(DateTime, "GMT");
from_datetime(DateTime, "+0000") ->
    format_rfc822(DateTime, "+0000");
from_datetime(DateTime, "-0000") ->
    format_rfc822(DateTime, "+0000");
from_datetime(DateTime0, Z) ->
    Offset = get_offset(Z),
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime0),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds - Offset),
    format_rfc822(DateTime, Z).

-spec from_timestamp(erlang:timestamp()) -> string().
from_timestamp(Timestamp) ->
    from_timestamp(Timestamp, ?DEFAULT_ZONE).

-spec from_timestamp(erlang:timestamp(), string()) -> string().
from_timestamp(Timestamp, Z) ->
    from_datetime(calendar:now_to_universal_time(Timestamp), Z).

-spec from_system_time(integer()) -> string().
from_system_time(Seconds) ->
    from_system_time(Seconds, ?DEFAULT_ZONE).

-spec from_system_time(integer(), string()) -> string().
from_system_time(Seconds, Z) ->
    from_datetime(calendar:system_time_to_universal_time(Seconds, second), Z).

parse_datetime({ok, [_DoW, Day, Mon, Year, H, M, S, "GMT"], _}) ->
    {{{Year, mon(Mon), Day}, {H, M, S}}, 0};
parse_datetime({ok, [_DoW, Day, Mon, Year, H, M, S, "+0000"], _}) ->
    {{{Year, mon(Mon), Day}, {H, M, S}}, 0};
parse_datetime({ok, [_DoW, Day, Mon, Year, H, M, S, "-0000"], _}) ->
    {{{Year, mon(Mon), Day}, {H, M, S}}, 0};
parse_datetime({ok, [_DoW, Day, Mon, Year, H, M, S, Z], _}) ->
    {{{Year, mon(Mon), Day}, {H, M, S}}, get_offset(Z)}.

get_offset(Z) ->
    parse_offset(io_lib:fread("~1s~2d~2d", Z)).

parse_offset({ok, ["+", Hrs, Min], _}) ->
    -3600*Hrs - 60*Min;
parse_offset({ok, ["-", Hrs, Min], _}) ->
    3600*Hrs + 60*Min.

parse_rfc822(Str) ->
    parse_datetime(io_lib:fread("~3s, ~2d ~3s ~4d ~2d:~2d:~2d ~s", Str)).

to_datetime(Str) ->
    case parse_rfc822(Str) of
        {DateTime, 0} -> DateTime;
        {DateTime, Offset} ->
            Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
            calendar:gregorian_seconds_to_datetime(Seconds + Offset)
    end.

gregorian_seconds_to_system_time(Seconds) ->
    Seconds - 62167219200. % gregorian seconds at 1970/01/01 00:00:00

-spec to_system_time(list()) -> integer().
to_system_time(Str) ->
    {DateTime, Offset} = parse_rfc822(Str),
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    gregorian_seconds_to_system_time(Seconds + Offset).

-spec to_timestamp(list()) -> erlang:timestamp().
to_timestamp(Str) ->
    Seconds = to_system_time(Str) * 1000000,
    MegaSecs = Seconds div 1000000000000,
    Secs = Seconds div 1000000 - MegaSecs*1000000,
    MicroSecs = Seconds rem 1000000,
    {MegaSecs, Secs, MicroSecs}.
