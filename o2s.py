# -------------------- Some regex combinators --------------------

import re

re_sub_ = lambda pat, repl, count=0: lambda s: re.sub(pat, repl(), s, count=count)
re_sub = lambda pat, repl, count=0: lambda s: re.sub(pat, repl, s, count=count)

def seq(*fs):
  def seqd(s):
    for f in fs:
      s = f(s)
    return s
  return seqd

sub = lambda old, new: seq(
  re_sub(f'^{old}(\\s)', f'{new}\\1'),
  re_sub(f'(\\(|\\s){old}(\\s|\\)|,)', f'\\1{new}\\2'))

def fix(f):
  def fixd(s):
    old = s
    while True:
      s = f(s)
      if old == s: return s
      else: old = s
  return fixd

def mk_fresh():
  i = 0
  def fresh():
    nonlocal i
    i += 1
    return 'x___fresh___' + str(i - 1)
  return fresh
fresh = mk_fresh()

ident = '[_a-zA-Z0-9\']+'
tyvar = f'\'{ident}'
tyvars = f'\\({tyvar}(,\\s*{tyvar})*\\)|{tyvar}|'

# -------------------- The translation --------------------

import sys
with open(sys.argv[1]) as f:
  code = f.read()

# Step 1: delete `module type` declarations and dedent `module` definitions

def indent_size(s): return len(s) - len(s.lstrip())

code = code.split('\n')
code_ = []
indent = 0
sig_level = None
while code != []:
  line, *code = code
  indent_ = indent_size(line)
  if line.lstrip().startswith('module type '):
    assert code[0].strip() == 'sig'
    sig_level = indent_size(code[1])
    code = code[2:]
  elif sig_level is not None and (indent_ >= sig_level or line.strip() == ''):
    continue
  elif sig_level is not None and indent_ < sig_level:
    sig_level = None
    indent = indent_
  elif line.strip() == 'struct':
    code_.append('struct')
    indent = indent_size(code[0])
  elif line.strip() == '':
    code_.append(line)
  else:
    indent = min(indent_size(line), indent)
    code_.append(line[indent:])

code = '\n'.join(code_)
del code_

# Step 2: adjust misc. syntactic constructs (e.g. match .. with -> case .. of)

code = 'type box___ = unit\nval box___ = ()\n\n' + seq(
  # delete Obj.* preamble
  sub('type __ = Obj.t\nlet __ = let rec f _ = Obj.repr f in Obj.repr f\n', ''),
  # match .. with --> case .. of
  fix(sub('match', 'case')),
  re_sub('(\\s)with(\\s*)\\|', '\\1of\\2 '),
  fix(sub('with', 'of')),
  # Fix arrows
  sub('->', '=>'),
  # type --> datatype if defining inductive type
  re_sub(
    f'type(\\s)+({tyvars})(\\s)*({ident})(\\s*)=(\\s*)\\|',
    'datatype\\1\\2\\4\\5\\6=\\7 '),
  # delete type aliases
  re_sub('\ntype[^\b]*?=[^\b]*?\n(let|module|type|datatype)', '\n\\1'),
  # (fun --> fn) and (function --> fn x => case x of) with x fresh
  sub('\\(fun', '(fn'),
  fix(re_sub_(
    'function(\\s*)\\|',
    lambda: (lambda x: f'fn {x} => case {x} of\\1 ')(fresh()),
    count=1)),
  # (let rec? f x .. = ..) --> (val rec? f = fn x => .. => ..)
  fix(re_sub(
    f'(let rec|and|let(?!\\s+rec\\s+))((\\s+{ident})+)(\\s+{ident}\\s*)=',
    '\\1\\2 = fn\\4=>')),
  sub('let', 'val'),
  # non-toplevel (val .. in val .. in ..) --> (let val .. in val .. in ..)
  re_sub('(?<! in)(\\s+|\\()(?<!\\n)val(\\s+)', '\\1let val\\2'),
  # (let val .. in val .. in ..) --> (let val .. val .. in ..)
  # This is not _strictly_ necessary but it reduces the number of `end`s that need to be added.
  re_sub('in(\\s+)val', '  \\1val'),
  re_sub('in(\\s+)let(\\s+)val', '  \\1   \\2val'),
  # Misc. fixups
  re_sub(f'module type(\\s+{ident}\\s*)=(\\s*)sig', 'signature\\1=\\2sig'),
  re_sub(f'module(\\s+{ident}\\s*)=(\\s*)struct', 'structure\\1=\\2struct'),
  sub('assert false', 'raise Match'),
  fix(sub('__', 'box___')),
)(code)

# To insert `end`: repeatedly query SMLNJ for parse errors and fix them.
# Action depends on location of error:
# - If:
#   - error is at column 1 and the text at that location is a keyword,
#   - error is at end of a line,
#   - error is elsewhere and text at that location is `)`,
#   - error is at EOF,
#   insert `end` at the location mentioned by SMLNJ.
# - Otherwise, the error should not be on column 1 and the text at that location should be `(`.
#   In this case skip forward and insert `end` immediately after the matching `)`.

temp_file = 'temp_out.sml' # File to be read by SMLNJ
debug = True # Print diffs at each step

# code : List[String], row col : int -> (row', col') : int * int
# Get next row-col coord
def next_rc(code, row, col):
  if col == len(code[row]) - 1:
    return row + 1, 0
  else:
    return row, col + 1

# code : List[String], row col : int -> (row', col') : int * int
# Find (row', col') of paren balancing (row, col)
def find_balancer(code, row, col):
  count = 1
  while count != 0:
    row, col = next_rc(code, row, col)
    if code[row][col] == ')':
      count -= 1
    elif code[row][col] == '(':
      count += 1
  return row, col

while True:
  with open(temp_file, 'w') as f:
    f.write(code)
  import subprocess
  p = subprocess.Popen(
    ['sml', temp_file],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE)
  p.stdin.close() # Kill the repl in the case where reading temp_file succeeded
  try:
    ss = [s.decode('utf-8') for s in p.stdout.readlines()]
    s = next(s[len(temp_file)+1:] for s in ss if s.startswith(temp_file))
    sp = s.find(' ')
    (row, col), s = tuple(map(int, s[:sp].split('.'))), s[sp+1:]
    print(row, col, s, end='') if debug else None
    row -= 1
    col -= 1
    code = code.split('\n')
    new_row = code[row]
    if s.strip().endswith('EOF'):
      print('\033[91m-', code[row], '\033[0m') if debug else None
      code[row] = code[row] + ' end'
      print('\033[92m+', code[row], '\033[0m') if debug else None
    elif col == 0 and (
        code[row].startswith('val ') or
        code[row].startswith('signature ') or
        code[row].startswith('structure ') or
        code[row].startswith('datatype ')):
      print('\033[91m-', code[row], '\033[0m') if debug else None
      code[row] = 'end ' + code[row]
      print('\033[92m+', code[row], '\033[0m') if debug else None
    elif code[row][col] == ')':
      print('\033[91m-', code[row], '\033[0m') if debug else None
      code[row] = code[row][:col] + ' end' + code[row][col:]
      print('\033[92m+', code[row], '\033[0m') if debug else None
    elif code[row][col] == '(':
      row_, col_ = find_balancer(code, row, col)
      print('\033[91m-', code[row_], '\033[0m') if debug else None
      code[row_] = code[row_][:col_+1] + ' end' + code[row_][col_+1:]
      print('\033[92m+', code[row], '\033[0m') if debug else None
    elif col == len(code[row]) - 1:
      print('\033[91m-', code[row], '\033[0m') if debug else None
      code[row] = code[row] + ' end'
      print('\033[92m+', code[row], '\033[0m') if debug else None
    else:
      print(code[row]) if debug else None
    code = '\n'.join(code)
  except StopIteration:
    import os
    os.remove(temp_file)
    break

print(code)
