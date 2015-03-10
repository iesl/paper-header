from bs4 import BeautifulSoup

class Author:
	def __init__(self, first, last, middle=None):
		self.first = first
		self.last = last
		self.middle = middle

	def toTSV(self):
		first = self.BILOU(self.first, "authorfirst")
		middle = None
		if self.middle is not None:
			middle = self.BILOU(self.middle, "authormiddle")
		last = self.BILOU(self.last, "authorlast")
		whole = first
		if middle is not None:
			whole += middle
		whole += last
		return whole

	def BILOU(self, stuff, tag):
		thing = ""
		if len(stuff) > 1:
			thing = "B-%s\t%s\n" % (tag, stuff[0])
			if len(thing) > 2:
				for i in range(1, len(stuff)-1):
					thing += "I-%s\t%s\n" % (tag, stuff[i])
			thing += "L-%s\t%s\n" % (tag, stuff[-1])
		else:
			thing = "U-%s\t%s\n" % (tag, stuff[0])
		return thing


	def __repr__(self):
		s = self.first + " "
		if self.middle is not None:
			s += self.middle + " "
		s += self.last
		return s


def process(filename):
	raw = open(filename, "r").read()
	docs = raw.split("\n")
	annots = []
	for d in docs:
		soup = BeautifulSoup(d)
		authors = soup.find_all("authors")
		for a in authors:
			ppl = a.find_all("person")
			for p in ppl:
				# print("person: ", p)
				firsts = p.find_all('person-first')
				middles = p.find_all('person-middle')
				lasts = p.find_all('person-last')
				annot = Author(None, None, middle=None)
				if len(firsts) > 0:
					annot.first = firsts[0].contents
				if len(middles) > 0:
					annot.middle = middles[0].contents
				if len(lasts) > 0:
					annot.last = lasts[0].contents
				# print(annot.first, annot.middle, annot.last)
				print(annot.toTSV())
		break

if __name__ == "__main__":
	import sys
	usage = "usage: python mung_citation.py [files]"
	if len(sys.argv) < 3:
		print(usage)
		sys.exit(1)
	files = sys.argv[1:]
	print(files)
	for f in files:
		process(f)