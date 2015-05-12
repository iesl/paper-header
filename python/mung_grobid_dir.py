import os, sys

usage = "usage: python mung_grobid_dir.py [dir]"
if len(sys.argv) != 2:
	print(usage)
	sys.exit(1)
dir = sys.argv[1].strip("/")+"/"
files = [dir+f for f in os.listdir(dir) if f.endswith(".tei")]
print("processing:", len(files), "files")
for f in files:
	cmd = "python python/mung_grobid.py " + f
	# print(cmd)
	os.system(cmd)