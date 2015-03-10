from bs4 import BeautifulSoup
import re
import codecs
import os

"""
FIXME line len not 2: I-affiliation			University in file: /Users/kate/research/citez/paper-header/grobid-data/header366.tei.clean
TODO double check all the tags are there / in the right format
"""

tagSet = {
	'docauthor': 'author',
	'titlepart': 'title',
	'affiliation': 'affiliation',
	'address': 'address',
	# 'note': 'note'
	# 'date'
	'email': 'email',
	# 'abstract': 'abstract',
	# 'phone': 'phone',
	'keywords': 'keyword',
	'ptr': 'web',
	'date': 'date'
}

divTags = ['abstract']
noteTags = ['phone']

lbPattern = re.compile(ur'<\/?lb>+', re.UNICODE)
punctPattern = re.compile(ur'([\.,\?!\"\'\(\)\:\-])', re.UNICODE)

def process_annot(annot):
	clean = clean_annot(annot)
	tsv = annot_to_tsv(clean)
	return tsv

def clean_annot(annot):
	assert len(annot) == 2
	tag, contents = annot[0], annot[1]
	clean = []
	for c in contents:
		# m = re.match(r'<\/?lb>+', str(c))
		m = re.match(lbPattern, unicode(c))
		if m:
			clean.append('\n')
		else:
			tokens = c.split(" ")
			toks = []
			for t in tokens:
				toks.extend(re.split(punctPattern, t))
			tokens = map(lambda x: x.strip("\n"), toks)
			tokens = filter(lambda x: len(x) > 0, tokens)
			clean.extend(tokens)
	return (tag, clean)

def annot_to_tsv(annot):
	tag, contents = annot
	contents = filter(lambda x: x != '\n', contents)
	if len(contents) == 0:
		return []
	if len(contents) == 1:
		bilou_tag = "U-" + tag
		contents = contents[0]
		return ["\t".join([bilou_tag, contents])]
	stuff = []
	stuff.append("\t".join(["B-" + tag, contents[0]]))
	last = "\t".join(["L-" + tag, contents[-1]])
	if len(contents) > 2:
		inside = []
		for i in range(1, len(contents)-1):
			inside.append("\t".join(["I-" + tag, contents[i]]))
		stuff.extend(inside)
	stuff.append(last)
	return stuff

def write_tsv(filename, annots):
	# print("writing:", filename)
	with codecs.open(filename, "w", "utf8") as f:
		for seg in annots:
			for a in seg:
				f.write(a + "\n")

def collect_stats(annots):
	fullTagSet = tagSet.values() + divTags + noteTags
	tagCounts = {x: 0.0 for x in fullTagSet}
	for a in annots:
		for seg in a:
			# print seg
			# sys.exit(1)
			# btag, contents = seg.split("\t")
			parts = filter(lambda x: len(x) > 0, seg.split(u'\t'))
			assert len(parts) == 2, "bad line (len %d != 2): %s" % (len(parts), seg)
			btag = parts[0]
			tag = btag.split("-")[1]
			tagCounts[tag] += 1.0
	tokenCount = sum(tagCounts.values())
	return (tokenCount, tagCounts)
		

def process(filename):
	print "processing: ", filename
	annots = []
	raw = codecs.open(filename, "r", "utf8").read()
	soup = BeautifulSoup(raw)
	for (grobid_tag, tag) in tagSet.items():
		results = soup.find_all(grobid_tag)
		for r in results:
			annots.append((tag, r.contents))
	divs = soup.find_all('div')
	for d in divs:
		for t in divTags:
			if d.has_attr('type') and d['type'] == t:
				annots.append((t, d.contents))
	notes = soup.find_all('note')
	for d in notes:
		for t in noteTags:
			if d.has_attr('type') and d['type'] == t:
				annots.append((t, d.contents))
	clean = []
	for a in annots:
		processed = process_annot(a)
		if len(processed) > 0:
			clean.append(processed)
		# clean.append(process_annot(a))
	write_tsv(filename+".clean", clean)
	return collect_stats(clean)

def process_dir(dir):
	dir = dir.strip("/") + "/"
	files = [dir+f for f in os.listdir(dir) if f.endswith(".tei")]
	print("processing:", len(files), "files")
	docCount, tokenCount = 0, 0
	global tagSet
	fullTagSet = tagSet.values() + divTags + noteTags
	tagCounts = {x: 0.0 for x in fullTagSet}
	for f in files:
		tc, tags = process(f)
		docCount += 1
		tokenCount += tc
		for (k, v) in tags.items():
			tagCounts[k] += v
	print("doc count:", docCount)
	print("token count:", tokenCount)
	for (k, v) in tagCounts.items():
		print(k, v)

if __name__ == "__main__":
	import sys
	usage = "usage: python mung_grobid.py [-f|-d] [target]"
	if len(sys.argv) != 3:
		print(usage)
		sys.exit(1)
	flag = sys.argv[1]
	target = sys.argv[2]
	if flag == "-d":
		print "processing dir: ", target
		process_dir(target)
	elif flag == "-f":
		print "processing file: ", target
		process(target)
	else:
		print(usage)
		sys.exit(1)

