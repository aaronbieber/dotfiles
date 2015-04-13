#!/usr/bin/env python

import sys
import json
import subprocess
import re
import os
import time
import calendar

def get_governor():
    """ Get the current governor for cpu0, assuming all CPUs use the same. """
    with open('/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor') as fp:
        return fp.readlines()[0].strip()

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
        j.insert(0, {'full_text' : '%s' % get_weather(), 'name' : 'weather'})
        j.insert(0, {'full_text' : '%s' % get_music(), 'name' : 'music'})
        # and echo back new encoded json
        print_line(prefix+json.dumps(j))
