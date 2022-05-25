#!/bin/bash

cargo +nightly bench

rm -r report
cp -r target/criterion report

shopt -s globstar
perl -i -pe 's|>generator-benchmark/\d\d |>|' report/**/*.svg report/**/*.html
cp report/generator-benchmark/report/violin.svg report/generator-benchmark/report/violin-dark.svg
sed -i 's/black/white/g' report/generator-benchmark/report/violin-dark.svg
