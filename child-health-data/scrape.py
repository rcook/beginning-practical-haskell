#!/usr/bin/env python
import bs4
import collections
import contextlib
import os
import unidecode
import urllib2
import xlsxwriter

from scrapelib.util import unpack_args

SCRIPT_DIR = os.path.abspath(os.path.dirname(__file__))

# {0}: QQ
# {1}: GG
URL_FORMAT_0 = "http://childhealthdata.org/browse/survey/results?q={0}&r=1&g={1}"
URL_FORMAT_1 = "http://childhealthdata.org/browse/survey/results?q={0}&r=1"

CACHE_DIR = ".cache"

QUESTION_SUFFIX = " (details)"

COLUMNS = [
  "question",
  "answer",
  "subgroup category",
  "subgroup value",
  "%",
  "CI - lower",
  "CI - upper",
  "n"
]

class Subcategory(object):
  def __init__(self, value, ps, cs, ns):
    self._value = value
    self._ps = ps
    self._cs = cs
    self._ns = ns

  @property
  def value(self): return self._value

  @property
  def ps(self): return self._ps

  @property
  def cs(self): return self._cs

  @property
  def ns(self): return self._ns

class Page(object):
  def __init__(self, qq, gg, question, answers, subcategories):
    self._qq = qq
    self._gg = gg
    self._question = question
    self._answers = answers
    self._subcategories = subcategories

  @property
  def qq(self): return self._qq

  @property
  def gg(self): return self._gg

  @property
  def question(self): return self._question

  @property
  def answers(self): return self._answers

  @property
  def subcategories(self): return self._subcategories

def make_path(*paths):
  return os.path.abspath(os.path.join(*paths))

def fetch_qqs():
  return map(int, open(make_path(SCRIPT_DIR, "qqs.txt"), "rt").readlines())

def fetch_ggs():
  lines = open(make_path(SCRIPT_DIR, "ggs.txt"), "rt").readlines()
  ggs = collections.OrderedDict()
  for line in lines:
    gg_temp, caption_temp = line.split(" ", 1)
    ggs[int(gg_temp)] = caption_temp.strip()

  return ggs

def fetch_page(qq, gg):
  path = make_path(SCRIPT_DIR, CACHE_DIR, "_{}_{}".format(qq, gg))
  if os.path.isfile(path):
    return open(path, "rt").read()

  url = (URL_FORMAT_1 if gg == -1 else URL_FORMAT_0).format(qq, gg)
  try:
    response = urllib2.urlopen(url)
  except urllib2.HTTPError as e:
    if e.code != 500:
      raise e
    content = ""
  else:
    content = response.read()

  with open(path, "wt") as f:
    f.write(content)

  return content

def parse_question(s):
  return s[: -len(QUESTION_SUFFIX)] if s.endswith(QUESTION_SUFFIX) else s

def parse_p(s):
  return float(s)

def parse_c(s):
  t0 = s.lstrip("(").rstrip(")")
  t1, t2 = t0.split("-")
  return float(t1), float(t2)

def parse_n(s):
  return int(s.replace(",", ""))

def fix(s):
  return unidecode.unidecode(s)

def parse_page(content):
  doc = bs4.BeautifulSoup(content, "lxml")
  table = doc.body.find(id="PageContent_PageContent_C001_tblResults")
  question = parse_question(fix(table.find("caption").text).strip())

  answers = map(lambda e: fix(e.text).strip(), table.find("thead").find("tr").find_all("th", {"class": "result"}))[: -1]

  trs = table.find("tbody").find_all("tr")
  i = iter(trs)

  subcategories = []
  while True:
    tr = next(i, None)
    if tr is None:
      break

    compare_th = tr.find("th", {"class": "compare"})
    value = fix(compare_th.text).strip() if compare_th else "all"

    ps = map(lambda e: parse_p(e.text), tr.find_all("span", {"class": "type_p"}))

    tr = next(i)
    cs = map(lambda e: parse_c(e.text), tr.find_all("span", {"class": "type_c"}))

    tr = next(i)
    ns = map(lambda e: parse_n(e.text), tr.find_all("span", {"class": "type_n"}))

    tr = next(i)

    subcategories.append(Subcategory(value, ps, cs, ns))

  return Page(qq, gg, question, answers, subcategories)

class WorksheetRowWriter(object):
  def __init__(self, worksheet):
    self._worksheet = worksheet
    self._row = 0

  def write_row(self, *args):
    columns = unpack_args(*args)
    for i, column in enumerate(columns):
      self._worksheet.write(self._row, i, column)
    self._row += 1

@contextlib.contextmanager
def worksheet_writer(path, worksheet_name):
  with xlsxwriter.Workbook(path) as workbook:
    worksheet = workbook.add_worksheet(worksheet_name)
    yield WorksheetRowWriter(worksheet)

qqs = fetch_qqs()
ggs = fetch_ggs()

pages = []
for qq in qqs:
  for gg in ggs.keys():
    print("qq={} gg={}".format(qq, gg))
    content = fetch_page(qq, gg)
    if len(content) > 0:
      pages.append(parse_page(content))

with worksheet_writer("output.xlsx", "Scrape") as w:
  w.write_row(COLUMNS)
  for page in pages:
    for subcategory in page.subcategories:
      for i, answer in enumerate(page.answers):
        p = subcategory.ps[i]
        c_lower, c_upper = subcategory.cs[i]
        n = subcategory.ns[i]
        w.write_row([
          page.question,
          answer,
          ggs[page.gg],
          subcategory.value,
          p,
          c_lower,
          c_upper,
          n
        ])
