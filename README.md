# Wyrd "flexible formats" fork

Wyrd is a curses front-end to remind. It is written in OCaml.

This repository is based off the Debian source for Wyrd version 1.4.6;
I could not find a reachable, public repository.

While the mainline version of Wyrd allows configuring a bunch of
strings in the rc file `~/.wyrdrc`, for example `set
timed_template="REM %monname% %mday% %year% AT %hour%:%min% DURATION
1:00 MSG "`, it does not allow configuring the exact expansions for
`%monname%` and friends. This is especially annoying for `%mday%`,
which is hard-coded to the equivalent of `printf "%d"`; lines written
in the first nine days of a month and the following ones will not line
up properly:

```
REM Jul 8 2017 AT 08:00 DURATION 1:00 MSG The month starts!
REM Jul 15 2017 AT 08:00 DURATION 1:00 MSG Mid-month ;_;
```

The present version of Wyrd adds format specifiers `monname_fmt`,
`mon_fmt`, `mday_fmt`, `year_fmt`, `hour_fmt`, `min_fmt`,
`wdayname_fmt`, and `wday_fmt`. When configuring the format for
`%wday%` as `set mday_fmt = "%2d"`, the two timestamps above will
instead be rendered as follows:

```
REM Jul  8 2017 AT 09:00 DURATION 1:00 MSG The month starts!
REM Jul 15 2017 AT 08:00 DURATION 1:00 MSG Mid-month ^_^
```

Observe how the `MSG` columns now line up nicely. **As they should.**
