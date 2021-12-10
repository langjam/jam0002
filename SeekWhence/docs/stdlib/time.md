# time - Time processing utilities

**Documentation Table of Contents**
- [Basic Syntax](./basic-syntax.md)
- [Sequences and Slices](./sequences.md)
- [Namespaces and Imports](./namespaces.md)
- Standard Library
  - [builtin](./stdlib/builtin.md) - Core functions and constants
  - [math](./stdlib/math.md) - Additional math functions and constants
  - [strutils](./stdlib/strutils.md) - String manipulation utilities
  - [time](./stdlib/time.md) - Time processing utilities

## Import
```racket
import time from 'time'
```


## Time Breakdown Functions

This module contains two namespaces, `utc` and `local`, for working with UTC and local time respectively. The following functions are available as children of each namespace. Manually selected timezones are currently not supported.

All functions expect the Unix time in milliseconds as a parameter, which can be retrieved with the builtin `[systime]` function.

| Name | Arguments | Description |
| :--- | :-------- | :---------- |
| `year` | `t` Unix time | The full year (e.g. 1993). |
| `month` | `t` Unix time | The month as a number (0-11). |
| `day` | `t` Unix time | The day of the month as a number (1-31). |
| `weekday` | `t` Unix time | The day of the week as a number (0-6). |
| `yearday` | `t` Unix time | The day of the year as a number (1-366). |
| `hour` | `t` Unix time | The hour as a number (0-23). |
| `minute` | `t` Unix time | The minute as a number (0-59). |
| `second` | `t` Unix time | The second as a number (0-61). |
| `isDST` | `t` Unix time | `true` if currently in Daylight Savings Time.<br>`none` if DST status can't be determined. |
| `timezone` | `t` Unix time | The timezone abbreviation. |
| `utcOffset` | `t` Unix time | The offset from UTC in milliseconds. |
| `format` | `t` Unix time<br>`f` Format string | Return the time formatted as a string, according to the given `f`. |


### Format String Directives

| Directive | Description |
| :--- | :-------- |
| `%a` | Locale’s abbreviated weekday name. |
| `%A` | Locale’s full weekday name. |
| `%b` | Locale’s abbreviated month name. |
| `%B` | Locale’s full month name. |
| `%c` | Locale’s appropriate date and time representation. |
| `%d` | Day of the month as a decimal number (01-31). |
| `%H` | Hour (24-hour clock) as a decimal number (00-23). |
| `%I` | Hour (12-hour clock) as a decimal number (01-12). |
| `%j` | Day of the year as a decimal number (001-366). |
| `%m` | Month as a decimal number (01-12). |
| `%M` | Minute as a decimal number (00-59). |
| `%p` | Locale’s equivalent of either AM or PM. |
| `%S` | Second as a decimal number (00-61). |
| `%U` | Week number of the year (from the first Sunday) as a decimal number (00-53). |
| `%w` | Weekday (from Sunday) as a decimal number (0-6). |
| `%W` | Week number of the year (from the first Monday) as a decimal number (00-53). |
| `%x` | Locale’s appropriate date representation. |
| `%X` | Locale’s appropriate time representation. |
| `%y` | Year without century as a decimal number (00-99). |
| `%Y` | Year with century as a decimal number. |
| `%z` | Time zone offset from UTC/GMT of the form +HHMM or -HHMM. |
| `%Z` | Time zone name or abbreviation (no characters if no time zone exists). |
| `%%` | A literal '%' character. |


## Constants

| Name | Description |
| :--- | :---------- |
| `epoch` | The epoch in Unix time (`0`). |
| `dayNames` | A sequence of English day of the week names (from Sunday). |
| `dayAbbrs` | A sequence of English day of the week abbreviations (from Sunday). |
| `monNames` | A sequence of English month names. |
| `monAbbrs` | A sequence of English month abbreviations. |
| `format~date_yymmdd` | Format string; date in year-month-day format, without century, separated by `/` |
| `format~date_ddmmyy` | Format string; date in day-month-year format, without century, separated by `/` |
| `format~date_mmddyy` | Format string; date in month-day-year format, without century, separated by `/` |
| `format~date_yyyymmdd` | Format string; date in year-month-day format, with century, separated by `/` |
| `format~date_ddmmyyyy` | Format string; date in day-month-year format, with century, separated by `/` |
| `format~date_mmddyyyy` | Format string; date in month-day-year format, with century, separated by `/` |
| `format~date_yymmdd_slash` | Format string; date in year-month-day format, without century, separated by `/` |
| `format~date_ddmmyy_slash` | Format string; date in day-month-year format, without century, separated by `/` |
| `format~date_mmddyy_slash` | Format string; date in month-day-year format, without century, separated by `/` |
| `format~date_yyyymmdd_slash` | Format string; date in year-month-day format, with century, separated by `/` |
| `format~date_ddmmyyyy_slash` | Format string; date in day-month-year format, with century, separated by `/` |
| `format~date_mmddyyyy_slash` | Format string; date in month-day-year format, with century, separated by `/` |
| `format~date_yymmdd_hyphen` | Format string; date in year-month-day format, without century, separated by `-` |
| `format~date_ddmmyy_hyphen` | Format string; date in day-month-year format, without century, separated by `-` |
| `format~date_mmddyy_hyphen` | Format string; date in month-day-year format, without century, separated by `-` |
| `format~date_yyyyddmm_hyphen` | Format string; date in year-month-day format, with century, separated by `-` |
| `format~date_ddmmyyyy_hyphen` | Format string; date in day-month-year format, with century, separated by `-` |
| `format~date_mmddyyyy_hyphen` | Format string; date in month-day-year format, with century, separated by `-` |
| `format~date_human_dmy_abbr` | Format string; human-readable date in day-month-year format, with abbreviated month name |
| `format~date_human_dmy_long` | Format string; human-readable date in day-month-year format, with full month name |
| `format~date_human_mdy_abbr` | Format string; human-readable date in month-day-year format, with abbreviated month name |
| `format~date_human_mdy_long` | Format string; human-readable date in month-day-year format, with full month name |
| `format~date_human_wdmy_abbr` | Format string; human-readable date in day-month-year format, with abbreviated month and weekday names |
| `format~date_human_wdmy_long` | Format string; human-readable date in day-month-year format, with full month and weekday names |
| `format~date_human_wmdy_abbr` | Format string; human-readable date in month-day-year format, with abbreviated month and weekday names |
| `format~date_human_wmdy_long` | Format string; human-readable date in month-day-year format, with full month and weekday names |
| `format~date_locale` | Format string; date in locale's default format |
| `format~time_24_hms` | Format string; time including second in 24 hour format |
| `format~time_12_hms` | Format string; time including second in 12 hour format |
| `format~time_24_hm` | Format string; time excluding second in 24 hour format |
| `format~time_12_hm` | Format string; time excluding second in 12 hour format |
| `format~time_locale` | Format string; time in locale's default format |
| `format~iso8601` | Format string; date and time in ISO 8601 format |
| `format~human_abbr` | Format string; human-readable date and time with abbreviated month and weekday names |
| `format~human_long` | Format string; human-readable date and time with full month and weekday names |
| `format~locale` | Format string; date and time in locale's default format |
