import os

color_map = {
	"institution": "gray",
	"address": "gray",
	"title": "red",
	"author": "orange",
	"tech": "gray",
	"date": "yellow",
	"note": "gray",
	"abstract": "green",
	"email": "blue",
	"thesis": "gray",
	"keyword": "gray"
}

# total: 445
NDOCS = 20
HEIGHT = 1402
WIDTH = 563

class Token:
	def __init__(self, parts):
		self.label = parts[0][2:]
		# if self.label == "title":
		# 	self.string = "title"
		# elif self.label == "abstract":
		# 	self.string = "abstract"
		# else:
		# 	self.string = "0"
		# self.string = parts[1]
		self.string = self.label
		self.y = 4386 + int(parts[3])
		self.x = int(parts[2])
		self.fs = 0
		if len(parts) == 5:
			self.fs = int(parts[4])
	def __repr__(self):
		return self.string
	def html(self):
		color = color_map[self.label]
		font = str(self.fs/10)
		if self.fs == -1:
			font = str(10)
		y = str(HEIGHT - (self.y/10))
		# y = str(self.y/10)
		x = str(self.x/10)
		# self.string = str(self.y)
		# string = self.string + " (" + str(self.y) + ")"
		string = self.string
		ndocs = float(NDOCS)
		opacity = str(ndocs / 100.0)
		html = '<text x="' + x + '" y="' + y + '" style="fill:'+color+';fill-opacity:'+opacity+';font-size:'+str(font)+'px;">'+string+'</text>SVG unsupported'
		return html

def load_docs(phroot):
	path = phroot+"/data/fullpaper-headers-modified.tsv"
	docs = []
	with open(path, "r") as f:
		curr = ""
		for line in f.readlines():
			# if len(docs) >= NDOCS:
				# break
			if line.startswith("#"):
				if len(curr) > 0:
					docs.append(curr)
				curr = ""
			else:
				curr += line
		docs.append(curr)
	return [get_lines(doc) for doc in docs]
	
def get_lines(raw):
	raw = raw.split("\n")
	lines = []
	firstLine = raw[0].strip('\n').split('\t')
	currYPos = int(firstLine[2])
	currLine = []
	for line in raw:
		if len(line) == 0:
			continue
		parts = line.strip('\n').split('\t')
		assert len(parts) == 5, "bad line: %s" % line
		y = int(parts[2])
		if y == currYPos:
			currLine.append(Token(parts))
		else:
			lines.append(currLine)
			currLine = [Token(parts)]
			currYPos = y
	lines.append(currLine)
	return lines

def get_lines_from_file(fname):
	raw = []
	with open(fname, 'r') as f:
		for line in f.readlines():
			raw.append(line)
	lines = []
	firstLine = raw[0].strip('\n').split('\t')
	currYPos = int(firstLine[2])
	currLine = []
	for line in raw:
		parts = line.strip('\n').split('\t')
		assert len(parts) == 5, "bad line: %s" % line
		y = int(parts[2])
		if y == currYPos:
			currLine.append(Token(parts))
		else:
			lines.append(currLine)
			currLine = [Token(parts)]
			currYPos = y
	lines.append(currLine)
	return lines

def gen_svg(fname):
	lines = get_lines(fname)
	html = ""
	for line in lines:
		labels = set(["title", "abstract"])
		# tokens = filter(lambda x: x.label in labels, line)
		tokens = line
		toks = [t.html() for t in tokens]
		for tok in toks:
			html += tok
			html += "\n"
	return html

def gen_svg_from_doc(lines):
	html = ""
	for line in lines:
		labels = set(["title", "abstract"])
		# tokens = filter(lambda x: x.label in labels, line)
		tokens = line
		toks = [t.html() for t in tokens]
		for tok in toks:
			html += tok
			html += "\n"
	return html

def superimpose_docs(docs):
	html = "<!DOCTYPE html><html><body>"
	for (k, v) in color_map.items():
		html += '<div style="font-size:10px;color:'+v+'">'+k+'</div>'
	props = 'width="' + str(WIDTH) + '" height="' + str(HEIGHT) + '"'
	html += '<svg ' + props + '>'
	html += '<rect ' + props + ' style="fill:white;stroke-width:3;stroke:rgb(0,0,0)" />'
	for doc in docs:
		html += gen_svg_from_doc(doc)
	html += "</svg>"
	html += "</body></html>"
	return html

def superimpose(files):
	html = "<!DOCTYPE html><html><body>"
	for (k, v) in color_map.items():
		html += '<div style="font-size:10px;color:'+v+'">'+k+'</div>'
	html += '<svg width="850" height="1100">'
	html += '<rect width="850" height="1100" style="fill:white;stroke-width:3;stroke:rgb(0,0,0)" />'
	for file in files:
		html += gen_svg(file)
	html += "</svg>"
	html += "</body></html>"
	return html

def gen_html_page(fname):
	html = "<!DOCTYPE html><html><body>"
	for (k, v) in color_map.items():
		html += '<div style="font-size:10px;color:'+v+'">'+k+'</div>'
	props = 'width="' + str(WIDTH) + '" height="' + str(HEIGHT) + '"'
	html += '<svg ' + props + '>'
	html += '<rect ' + props + ' style="fill:white;stroke-width:3;stroke:rgb(0,0,0)" />'
	lines = get_lines_from_file(fname)
	for line in lines:
		labels = set(["title", "abstract"])
		# tokens = filter(lambda x: x.label in labels, line)
		tokens = line
		toks = [t.html() for t in tokens]
		for tok in toks:
			html += tok
			html += "\n"
	html += "</svg>"
	html += "</body></html>"
	return html

def write_page(fname):
	html = gen_html_page(fname+".tsv")
	with open(fname+".html", "w") as f:
		f.write(html)

def get_stats(docs):
	labels = {}
	tokenCount = 0
	for doc in docs:
		for line in doc:
			for token in line:
				if token.label in labels:
					labels[token.label] += 1
				else:
					labels[token.label] = 1
				tokenCount += 1
	print("* doc count: ", len(docs))
	print("* token count: ", tokenCount)
	for (k, v) in labels.items():
		s = " ".join([k, str(round(1.0*v/tokenCount, 3)), str(v)])
		print(s)
		


docs = load_docs("/home/kate/research/paper-header")
get_stats(docs)

# html = superimpose_docs(docs[:10])
# with open("superimposed.html", "w") as f:
# 	f.write(html)

# write_page("examples/example")
# write_page("examples/example2")
# write_page("examples/example3")

# html = superimpose(["examples/example.tsv", "examples/example2.tsv", "examples/example3.tsv"])
# with open("superimposed.html", "w") as f:
# 	f.write(html)


	