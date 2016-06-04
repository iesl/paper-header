"""
Attempt to download original papers (pdfs) for annotated IESL paper header dataset
"""
import os
import sys

#SUFFIXES = {'.pdf', '.ps', '.gz'}

class Document(object):
    def __init__(self, identifier):
        self.identifier = identifier
        self.tokens = []
        self.labels = []

    def add_line(self, line):
        parts = line.rstrip('\n').split('\t')
        assert len(parts) >= 2, 'bad line: [%s]' % line
        label = parts[0]
        token = parts[1]
        self.tokens.append(token)
        self.labels.append(label)

    def url(self):
        if self.identifier.startswith('/home/timv'):
            parts = self.identifier.split('/')
            url_part = parts[-1]
            assert url_part.startswith('http') or url_part.startswith('ftp'), '%s' % url_part
            assert url_part.endswith('xml')
            url_string = url_part.replace('#', '/')
            if url_string.endswith('.pp.tagged.xml.timv.xml.timv2.xml'):
                url_string = url_string[:-33]
            assert url_string.endswith('.pdf') or url_string.endswith('.ps') or url_string.endswith('.gz')
            return url_string
        else:
            raise ValueError('not yet implemented')

    def __repr__(self):
        lines = []
        lines.append(self.identifier)
        for tok, label in zip(self.tokens, self.labels):
            lines.append('%s\t%s' % (tok, label))
        return '\n'.join(lines)


    def __str__(self):
        return self.__repr__()


def load_fullpapers_tsv(filename):
    """
    :param filename: fullpaper-headers.tsv (see data/README.md)
    :return:
    """
    docs = []
    doc = None
    with open(filename, 'r') as fr:
        for line in fr.readlines():
           line = line.rstrip('\n')
           if line.startswith('# file:'):
               if doc is not None:
                   docs.append(doc)
               ident = line[8:]  # strip off '# file: ' prefix (= 8 chars)
               doc = Document(ident)
           elif len(line) > 0:
               doc.add_line(line)
    print('loaded %d docs from %s' % (len(docs), filename))
    return docs


def wget(url, outdir, tries=1, timeout=10, wait=10):
    outfile = '%s/%s' % (outdir, url.replace('/', '#'))
    if os.path.exists(outfile):
        print('already downloaded %s' % outfile)
        return
    cmd = 'wget --tries=%d --timeout=%d --wait=%d --output-document=%s %s' % (tries, timeout, wait, outfile, url)
    print(cmd)
    os.system(cmd)



def download_papers(docs, outdir):
    for doc in docs:
        wget(doc.url(), outdir)
        print('')
    return None


if __name__ == '__main__':
    filename = sys.argv[1]
    outdir = sys.argv[2]
    docs = load_fullpapers_tsv(filename)
    download_papers(docs, outdir)



