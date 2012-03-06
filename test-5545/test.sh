#!/bin/bash

# First compilation, no bytecode present
rm -rf out 
mkdir out 
scalac -cp out -d out test.scala -Ylog:all -Ydebug -uniqid 2>&1 | grep -v '[log jvm] Index value for' >log1 

# Second compilation, bytecode present
rm -rf out
mkdir out
scalac -cp out -d out test.scala &>/dev/null
scalac -cp out -d out test.scala -Ylog:all -Ydebug -uniqid 2>&1 | grep -v '[log jvm] Index value for' >log2

