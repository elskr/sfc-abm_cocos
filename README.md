# Contingent Convertible Bonds and Macroeconomic Stability in a Stock-Flow Consistent Agent-Based Model

## Overview

This repository contains the code for the model presented in the paper "Contingent Convertible Bonds and Macroeconomic Stability in a Stock-Flow Consistent Agent-Based Model".

## Additional Contents

The folder "paper" contains:
- All the Plots Produced for the Baseline Validation of the model.
- The data specifically used to produce the results presented in the paper, obtained by running simulations.
- The R Scripts required to transform and analyze the results, and plot the figures.

## Prerequisites

Running the model requires Java version 6 or later.

Note that on Mac OS, you will need to use the Oracle version of Java instead of the default one shipped with the OS.

## Installation

The project archive can be imported directly into the Eclipse IDE as an existing project.

## Running the simulations from the Eclipse IDE

The distribution archive can be imported directly into the Eclipse IDE by using the File/Import menu item. Create a launch configuration in the benchmark project with the main class benchmark.Main and specify which configuration file you want to use by setting the system property jabm.config using the JVM argument -D , for example

-Djabm.config=model/mainBaseline.xml

## Documentation

The folder documentation in the <a href="https://github.com/S120/jmab">JMAB project</a> contains a user guide to the JMAB framework developped by Alessandro Caiani and Antoine Godin.

## Copyright Disclaimer 1

Copyright (c) 2016 Alessandro Caiani and Antoine Godin

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Copyright Disclaimer 2

Copyright (c) 2021 Elise Kremer

Permission is hereby granted, free of charge, to any person obtaining a copy of the additional code associated with the CoCo extension of JMAB and associated documentation files (the "CoCo Extension"), to deal in the CoCo Extension without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the CoCo Extension, and to permit persons to whom the CoCo Extension is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the CoCo Extension.

THE COCO EXTENSION IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE COCO EXTENSION OR THE USE OR OTHER DEALINGS IN THE COCO EXTENSION.
