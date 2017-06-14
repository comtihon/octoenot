# Coon load server
[![Build Status](https://travis-ci.org/comtihon/octocoon.svg?branch=master)](https://travis-ci.org/comtihon/octocoon)  
Gets create tag callback from github, clones this repo, builds and loads to artifactory coon repo.

Prototype.

# Build and run
    
    coon release
    _rel/octocoon/bin/octocoon start

# Requires:
* [coon](https://github.com/comtihon/coon) (should be installed locally)
* [artifactory](https://www.jfrog.com/artifactory/) (url is set in app.src)
* [sqlite3](https://www.sqlite.org/) (should be installed locally)