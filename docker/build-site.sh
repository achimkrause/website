#!/bin/bash
stack --allow-different-user --resolver=${STACK_RESOLVER} build
stack --allow-different-user --resolver=${STACK_RESOLVER} run -- clean
stack --allow-different-user --resolver=${STACK_RESOLVER} run -- build
