
import time

from core.sequence import Sequence

def buildTimeNS(struct_time):
  return {
    'year':    lambda _,t: struct_time(t/1000).tm_year,
    'month':   lambda _,t: struct_time(t/1000).tm_mon-1,
    'day':     lambda _,t: struct_time(t/1000).tm_mday-1,
    'weekday': lambda _,t: struct_time(t/1000).tm_wday,
    'yearday': lambda _,t: struct_time(t/1000).tm_yday,
    'hour':    lambda _,t: struct_time(t/1000).tm_hour,
    'minute':  lambda _,t: struct_time(t/1000).tm_min,
    'second':  lambda _,t: struct_time(t/1000).tm_sec,
    'isDST':   lambda _,t: [None,False,True][struct_time(t/1000).tm_isdst+1],
    'format':  lambda _,t,f: time.strftime(f, struct_time(t/1000)),
    'toCTime': lambda _,t: time.asctime(struct_time(t/1000)),

    'timezone':  lambda _,t: struct_time(t/1000).tm_zone,
    'utcOffset': lambda _,t: struct_time(t/1000).tm_gmtoff*1000
  }

SEQ_EXPORTS = {
  'epoch': 0,

  'utc': buildTimeNS(time.gmtime),
  'local': buildTimeNS(time.localtime),

  'dayNames': Sequence.build(ident = 'dayNames', exprs = ['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday']),
  'dayAbbrs': Sequence.build(ident = 'dayAbbrs', exprs = ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']),
  'monNames': Sequence.build(ident = 'monNames', exprs = ['January','February','March','April','May','June','July','August','September','October','November','December']),
  'monAbbrs': Sequence.build(ident = 'monAbbrs', exprs = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']),

  'formats': {
    'date_yymmdd':          '%Y/%m/%d',
    'date_ddmmyy':          '%d/%m/%y',
    'date_mmddyy':          '%m/%d/%y',
    'date_ddmmyyyy':        '%d/%m/%Y',
    'date_mmddyyyy':        '%m/%d/%Y',
    'date_yymmdd_slash':    '%Y/%m/%d',
    'date_ddmmyy_slash':    '%d/%m/%y',
    'date_mmddyy_slash':    '%m/%d/%y',
    'date_ddmmyyyy_slash':  '%d/%m/%Y',
    'date_mmddyyyy_slash':  '%m/%d/%Y',
    'date_yymmdd_hyphen':   '%Y-%m-%d',
    'date_ddmmyy_hyphen':   '%d-%m-%y',
    'date_mmddyy_hyphen':   '%m-%d-%y',
    'date_ddmmyyyy_hyphen': '%d-%m-%Y',
    'date_mmddyyyy_hyphen': '%m-%d-%Y',
    'date_human_dmy_abbr':  '%d %b %Y',
    'date_human_dmy_long':  '%d %B %Y',
    'date_human_mdy_abbr':  '%b %d, %Y',
    'date_human_mdy_long':  '%B %d, %Y',
    'date_human_wdmy_abbr': '%a, %d %b %Y',
    'date_human_wdmy_long': '%A, %d %B %Y',
    'date_human_wmdy_abbr': '%a, %b %d, %Y',
    'date_human_wmdy_long': '%A, %B %d, %Y',
    'date_locale':          '%x',

    'time_24_hms': '%H:%M:%S',
    'time_12_hms': '%I:%M:%S %p',
    'time_24_hm':  '%H:%M',
    'time_12_hm':  '%I:%M %p',
    'time_locale': '%X',

    'iso8601':    '%Y-%m-%dT%H:%M:%S%zZ',
    'human_abbr': '%a, %d %b %Y %H:%M:%S %Z',
    'human_long': '%A, %d %B %Y %H:%M:%S %Z',
    'locale':     '%c',
  }
}
