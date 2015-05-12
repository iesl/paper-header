from bs4 import BeautifulSoup
import codecs

"""
    "author",
    "institution",
    "title",
    "keyword",
    "date",
    "email",
    "address",
    "abstract",
    "note"
    """

class Citation:
	def __init__(self, raw):
		self.authors = []
		self.institution = []
		self.title = []
		self.keyword = []
		self.date = []
		self.email = []
		self.address = []
		self.raw = raw
		self.soup =  BeautifulSoup(self.raw)

	def process(self):
		self.get_authors()
		# print(self.authors)
		self.get_titles()
		# print(self.title)
		self.get_institution()
		# print(self.institution)
		self.get_date()
		# print(self.date)
		self.get_address()
		# print(self.address)

	def to_doc(self):
		string = ""
		for auth in self.authors:
			string += self.to_bio(auth, "author")
		for title in self.title:
			t = title[0].split(" ")
			t = filter(lambda x: len(x) > 0, t)
			string += self.to_bio(t, "title")
		for inst in self.institution:
			# print(inst)
			i = filter(lambda x: len(x) > 0, inst[0].split(" "))
			string += self.to_bio(i, "institution")
		for d in self.date:
			i = filter(lambda x: len(x) > 1, d[0].split(" "))
			if len(i) > 0:
				string += self.to_bio(i, "date")
		for d in self.address:
			i = filter(lambda x: len(x) > 1, d[0].split(" "))
			string += self.to_bio(i, "address")
		return string

	def to_bio(self, stuff, label):
		assert len(stuff) > 0, "empty: %s" % label
		s = "\t".join(["B-"+label, stuff[0]]) + "\n"
		for p in stuff[1:]:
			s += "\t".join(["I-"+label, p]) + "\n"
		return s


	def get_authors(self):
		auths = self.soup.find_all('authors')
		for auth in auths:
			for p in auth.find_all('person'):
				first = clean_auth(p.find_all('person-first'))
				middle = clean_auth(p.find_all('person-middle'))
				last = clean_auth(p.find_all('person-last'))
				name = [first]
				if len(middle) > 0:
					name.append(middle)
				name.append(last)
				self.authors.append(name)

	def get_titles(self):
		fields = ['title', 'booktitle']
		for f in fields:
			t = self.soup.find_all(f)
			for i in t:
				self.title.append(i.contents)
				
	def get_institution(self):
		fields = ['school', 'department']
		for f in fields:
			t = self.soup.find_all(f)
			for i in t:
				self.institution.append(i.contents)
	
	def get_date(self):
		fields = ['date']
		subfields = ['year', 'month']
		for f in fields:
			t = self.soup.find_all(f)
			for i in t:
				for s in subfields:
					sf = i.find_all(s)
					if len(sf) > 0:
						self.date.append(sf[0].contents)

	def get_address(self):
		fields = ['address']
		for f in fields:
			t = self.soup.find_all(f)
			for i in t:
				self.address.append(i.contents)
	

# http://www.ebscohost.com/titleLists/a2h-journals.htm
DATA_DIR = "/home/kate/research/data"
DATA_PATHS = {
	"citation": DATA_DIR + "/umass-citation"
}




def mung_citation_data():
	# # file: /home/timv/projects/textmill/RexaTextmill/../data/rexa-fullpaper/ghuang/ftp:##www.cs.yale.edu#pub#TR#tr1280.pdf.pp.tagged.xml.timv.xml.timv2.xml
	fname = DATA_PATHS["citation"] + "/training.docs"
	raw = open(fname, "r").read()
	cites = raw.split("\n")
	docs = []
	for cite in cites:
		c = Citation(cite)
		c.process()
		doc = c.to_doc()
		if len(doc) > 0 and len(c.title) > 0 and len(c.authors) > 0:
			docs.append(doc)
	with codecs.open("cites.tsv", "w", "utf8") as f:
		# f.write("\n")
		ct = 0
		for doc in docs:
			try:
				header = "# cite-doc-"+str(ct)+"\n"
				ct += 1
				f.write(header)
				f.write(doc)
				f.write("\n")
			except UnicodeEncodeError:
				continue


	# c = cites[0]
	# cit = Citation(c)
	# cit.process()
	# with open("test", "w") as f:
	# 	f.write(cit.to_doc())
	# # for cite in cites[:10]:
	# # 	c = Citation(cite)
	# # 	c.process()
	# # 	try:
	# # 		print(c.to_doc())
	# # 	except UnicodeEncodeError:
	# # 		pass
	# # 	print ""


def clean_auth(thing):
	if len(thing) > 0:
		return thing[0].contents[0].split(",")[0].strip(" ")
	else:
		return ""

def to_bio(string, label):
	parts = string.split(" ")
	parts = filter(lambda x: len(x) > 0, parts)
	bio = []
	bio.append("\t".join(["B-"+label, parts[0]]))
	for p in parts:
		bio.append("\t".join(["I-"+label, p]))
	return bio


mung_citation_data()


def extract():
	raw = open("a2h-journals.htm", "r").read()
	soup = BeautifulSoup(raw)
	rows = soup.table.find_all('tr')
	titles = []
	for r in rows:
		cols = r.find_all('td')
		ct = 0
		for c in cols:
			if ct == 2:
				# print(c.contents)
				titles.append(c.contents[0])
			ct += 1

	with open("titles1.txt", "w") as f:
		for t in titles:
			try:
				f.write(t+"\n")
			except UnicodeEncodeError:
				pass


def normalize():
	raw = []
	with open("titles1.txt", "r") as f:
		for line in f.readlines():
			raw.append(line.strip("\n"))
	titles = [t.lower() for t in raw]
	with open("titles.txt", "w") as f:
		for t in titles:
			f.write(t+"\n")
