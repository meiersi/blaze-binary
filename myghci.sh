#! /bin/bash

ghci -no-user-package-conf -package-conf cabal-dev/packages-7.0.3.conf/ $*
