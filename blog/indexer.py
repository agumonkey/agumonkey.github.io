#!/usr/bin/env python

import os

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

link = '<li><a href="%s">%s</a></li>'

entries = '\n'.join(link % (f,f) for f in os.listdir() if f.endswith('.html'))

def main():
    with open('index.html','w') as out:
        out.write(template % entries)

if __name__ == '__main__':
    main()
