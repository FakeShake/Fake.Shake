#!/usr/bin/env bash

mono paket.bootstrapper.exe
mono paket.exe restore
mono packages/FAKE/tools/FAKE.exe --nocache build.fsx
