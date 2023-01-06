#!/bin/bash
S="$1"
E="$2"
N="$3"
shift
ocamlopt -o "./$S/$E/$N" "./$S/$E/$N.ml" && ./"./$S/$E/$N"