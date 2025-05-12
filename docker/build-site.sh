#!/bin/bash
stack --allow-different-user build
stack --allow-different-user exec website clean
stack --allow-different-user exec website build
