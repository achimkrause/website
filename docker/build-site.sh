#!/bin/bash
stack build
stack --allow-different-user exec website clean
stack --allow-different-user exec website build
