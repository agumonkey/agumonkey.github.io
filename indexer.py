#!/usr/bin/env python

import os
import glob

template = '''
<html>
  <head>
    <title>blog</title>
  </head>
  <body>
    <h1>blog</h1>
    <ul>
%s
    </ul>
    <address>
      <a href="mailto:agumonkey@gmail.com"></a>
    </address>
  </body>
</html>
'''

blog = 'blog'
dest = 'dest'
match = os.path.sep.join([blog, dest, '*.html'])
link = '      <li><a href="%s">%s</a></li>'

entries = '\n'.join(link % (f,f)
                    for f in sorted(glob.glob(match))
                    if not f.endswith('index.html'))

def main():
    index = os.path.sep.join([blog, dest, 'index.html'])
    with open(index,'w') as out:
        out.write(template % entries)

if __name__ == '__main__':
    main()
