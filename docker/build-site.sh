#!/bin/bash
stack build
stack exec website clean
stack exec website build
