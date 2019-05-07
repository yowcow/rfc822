-module(rfc822).

-export([
    from_now/1,
    from_datetime/1,
    from_system_time/1
]).

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

from_datetime({{Year, Mon, Day}, {H, M, S}}) ->
    lists:flatten(io_lib:format("~s, ~2.10.0B ~s ~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B GMT", [
        day(calendar:day_of_the_week(Year, Mon, Day)),
        Day, month(Mon), Year,
        H, M, S
    ])).

from_now(Now) ->
    from_datetime(calendar:now_to_universal_time(Now)).

from_system_time(Time) ->
    from_datetime(calendar:system_time_to_universal_time(Time, second)).
