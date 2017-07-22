# Coon load server
[![Build Status](https://travis-ci.org/comtihon/octocoon.svg?branch=master)](https://travis-ci.org/comtihon/octocoon)  
Gets create tag callback from github, clones this repo, builds and loads to artifactory coon repo.

Prototype.

# Install from coon
You can install this service directly from coon:

    coon install octocoon

# Build and run
Clone this repo locally and run:

    coon release
    _rel/octocoon/bin/octocoon start

# Requires:
* [coon](https://github.com/comtihon/coon) (should be installed locally)
* [kerl](https://github.com/kerl/kerl) (should be installed locally)
* Erlang (at lease one release from kerl should be installed locally)
* [artifactory](https://www.jfrog.com/artifactory/) (url is set in app.src)
* [sqlite3](https://www.sqlite.org/) (should be installed locally)

# Security
* disable executing prebuild commands with `{disable_prebuild, true}`
* disable http access to `/statistics` for everyone except Collectd

# Monitoring
octocoon's metrics can be available on `localhost:4232/statistics` in JSON format.