
import os
import sys
import subprocess
import yaml
import time

from types import SimpleNamespace as dotdict

def indent(s):
  return '\n'.join(['  '+line for line in s.splitlines()])

def timeit(fn):
  start_time = time.time()
  value = fn()
  elapsed_time = time.time() - start_time
  return value, elapsed_time*1000

if __name__ == '__main__':
  if len(sys.argv) < 2:
    print('Expected test folder path as command line argument')

  testdir = sys.argv[1]
  failMark = '\u001B[1;31m✗\u001B[0m'
  succMark = '\u001B[1;32m✓\u001B[0m'

  failed, current_failed = False, False
  def fail(testname, msg, cf):
    if cf: print(indent(msg), file=sys.stderr)
    else:  print(f'{failMark} Test failed: {testname}\n{indent(msg)}')
    return True, True

  for filename in sorted(os.listdir(testdir)):
    if not filename.endswith('.yaml'): continue
    
    testname = os.path.splitext(os.path.basename(f'{testdir}/{filename}'))[0]
    current_failed = False

    try:
      with open(f'{testdir}/{filename}', 'r') as info:
        testinfo = yaml.safe_load(info)
        testinfd = dotdict(**testinfo)
        testsrc  = f'{testdir}/{testinfd.source}' if 'source' in testinfo else os.path.splitext(f'{testdir}/{filename}')[0]+'.seq'

        testproc, testtime = timeit(lambda: subprocess
          .run(['python3', f'{os.path.dirname(sys.argv[0])}/main.py', testsrc], capture_output=True, text=True, 
            input=(testinfd.stdin if 'stdin' in testinfo else '')))

        if 'status' in testinfo and testproc.returncode != testinfd.status:
          failed, current_failed = fail(testname, f'Expected exit code {testinfd.status}, got {testproc.returncode}', current_failed)

        if 'stdout' not in testinfo and len(testproc.stdout):
          failed, current_failed = fail(testname, f'Expected empty stdout, got\n{indent(testproc.stdout)}', current_failed)
        if 'stderr' not in testinfo and len(testproc.stderr):
          failed, current_failed = fail(testname, f'Expected empty stderr, got\n{indent(testproc.stderr)}', current_failed)

        if 'stdout' in testinfo and testproc.stdout != testinfd.stdout:
          failed, current_failed = fail(testname, f'Expected stdout:\n{indent(testinfd.stdout)}\nActual stdout:\n{indent(testproc.stdout)}', current_failed)
        if 'stderr' in testinfo and testproc.stderr != testinfd.stderr:
          failed, current_failed = fail(testname, f'Expected stderr:\n{indent(testinfd.stderr)}\nActual stderr:\n{indent(testproc.stderr)}', current_failed)

        if not current_failed:
          name = testname.ljust(10,' ')
          print(f'{succMark} Test passed: {name}\t(in %.4f ms)' % testtime)
    except Exception as e:
      failed, current_failed = fail(testname, e.message if 'message' in e.__dict__ else str(e), current_failed)

  if not failed:
    print(f'{succMark} All tests passed!')


