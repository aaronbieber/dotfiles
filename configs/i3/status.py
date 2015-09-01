#!/usr/bin/env python

import calendar
import json
import os
import re
import subprocess
import sys
import time

def tail(fname, lines = 1):
    bufsize = 8192
    fsize = os.stat(fname).st_size

    iter = 0
    with open(fname) as f:
        if bufsize > fsize:
            bufsize = fsize-1
        data = []
        while True:
            iter +=1
            f.seek(fsize-bufsize*iter)
            data.extend(f.readlines())
            if len(data) >= lines or f.tell() == 0:
                return ''.join(data[-lines:])

def is_syncd(repo):
    if repo == 'php':
        line = tail('/home/abieber/realsync/logs/php.log')
    elif repo == 'resources':
        line = tail('/home/abieber/realsync/logs/resources.log')
    elif repo == 'templates':
        line = tail('/home/abieber/realsync/logs/templates.log')

    return 'done' in line \
        or 'ready' in line \
        or 'speedup' in line \
        or 'Watching' in line

def get_sync_status():
    """Get the status of realsync for my three repos."""
    php = resources = templates = 'working'

    if is_syncd('php'):
        php = 'done'
    if is_syncd('resources'):
        resources = 'done'
    if is_syncd('templates'):
        templates = 'done'

    return 'PHP: %s, Resources: %s, Templates: %s' % (php, resources, templates)

def get_music():
    return subprocess.check_output(["/home/abieber/bin/show_track"])

def get_weather():
    weather_cache_file = os.path.expanduser("~/.i3/weather_cache")
    now = calendar.timegm(time.gmtime())

    if not os.path.exists(weather_cache_file) \
       or now - os.path.getmtime(weather_cache_file) > 900:
        weather = subprocess.check_output(["weather",
                                        "--imperial",
                                        "--cacheage=900",
                                        "KBOS"])
        with open(weather_cache_file, "w") as fp:
            fp.write("Cache touched %s\n" % str(os.path.getmtime(weather_cache_file)))
            fp.write("File is %s old.\n" % str(now - os.path.getmtime(weather_cache_file)))
            fp.write("Updated %s\n" % str(now))
            fp.write(weather)

    with open(weather_cache_file) as fp:
        weather = "\n".join(fp.readlines())

    m = re.search('Temperature: (.*)', weather)
    temp = m.group(1)
    m = re.search('Sky conditions: (.*)', weather)
    cond = m.group(1)

    return "%s - %s" % (temp, cond)

def print_line(message):
    """ Non-buffered printing to stdout. """
    sys.stdout.write(message + '\n')
    sys.stdout.flush()

def read_line():
    """ Interrupted respecting reader for stdin. """
    # try reading a line, removing any extra whitespace
    try:
        line = sys.stdin.readline().strip()
        # i3status sends EOF, or an empty line
        if not line:
            sys.exit(3)
        return line
    # exit on ctrl-c
    except KeyboardInterrupt:
        sys.exit()

if __name__ == '__main__':
    # Skip the first line which contains the version header.
    print_line(read_line())

    # The second line contains the start of the infinite array.
    print_line(read_line())

    while True:
        line, prefix = read_line(), ''
        # ignore comma at start of lines
        if line.startswith(','):
            line, prefix = line[1:], ','

        j = json.loads(line)
        # insert information into the start of the json, but could be anywhere
        # CHANGE THIS LINE TO INSERT SOMETHING ELSE
        j.insert(0, {'full_text': '%s' % get_weather(), 'name': 'weather'})
        j.insert(0, {'full_text': '%s' % get_sync_status(), 'name': 'syncd'})
        # j.insert(0, {'full_text' : '%s' % get_music(), 'name' : 'music'})
        # and echo back new encoded json
        print_line(prefix+json.dumps(j))
