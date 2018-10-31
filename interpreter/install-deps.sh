#!/bin/sh
mkdir -p ./.eggs/lib/chicken/8
chicken-install -init ./.eggs/lib/chicken/8
chicken-install -p .eggs
