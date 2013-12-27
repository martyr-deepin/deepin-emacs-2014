#!/bin/bash
 
# Add some options to make jslint shutup.
~/.npm/jslint/0.1.8/package/bin/jslint.js $@ -vars -eqeq -plusplus -browser -sloppy -white | sed 'N;s/\n/\I BLAME BOTH FLYMAKE AND JSLINT/'

