# Ecosmos Agent Guide

## Project Goal

- Create a very large evolution simulation running on an FPGA

## Project Layout

- `PL/` is the Clash/Haskell package (`Ecosmos.cabal`). `Example.Project.topEntity` is the HDL synthesis entrypoint.
- `Analysis/` contains python scripts to do general project estimations of for example performance and memory usage

## PL Workflow

- Run all PL tests with `just pl-test`.
- The shared Cabal options enable `NoImplicitPrelude` and Clash-specific GHC settings.
- Hardware modules should use `Clash.Prelude`, as the regular Haskell Prelude is only for SW
- Only code that is to be synthesized should be synthesizable. That is code in `PL/src`, while the testbenches in `PL/tests` do not have that restriction

