# Enot load server
[![Build Status](https://travis-ci.org/comtihon/octoenot.svg?branch=master)](https://travis-ci.org/comtihon/octoenot)  
Gets create tag callback from github, clones this repo, builds and loads to artifactory enot repo.

__Important__. This service is deprecated in favor for [Enot Auto Builder](https://github.com/comtihon/enot_auto_builder).

Prototype.

# Install from enot
You can install this service directly from enot:

    enot install octoenot

# Build and run
Clone this repo locally and run:

    enot release
    _rel/octoenot/bin/octoenot start

# Requires:
* [enot](https://github.com/comtihon/enot) (should be installed locally)
* [kerl](https://github.com/kerl/kerl) (should be installed locally)
* Erlang (at lease one release from kerl should be installed locally)
* [artifactory](https://www.jfrog.com/artifactory/) (url is set in app.src)
* [sqlite3](https://www.sqlite.org/) (should be installed locally)

# Security
* disable executing prebuild commands with `{disable_prebuild, true}`
* disable http access to `/statistics` for everyone except Collectd

# Monitoring
octoenot's metrics can be available on `localhost:4232/statistics` in JSON format.
