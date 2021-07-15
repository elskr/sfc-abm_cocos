# Contingent Convertible Bonds and Macroeconomic Stability in a Stock-Flow Consistent Agent-Based Model

## Overview

The folder contains the model-specific code empolyed for the model presented in the paper "Contingent Convertible Bonds and Macroeconomic Stability in a Stock-Flow Consistent Agent-Based Model" and additional materials to run simulations by your own and analyze results.

The code requires and builds upon the JMAB platform (<a href="https://github.com/S120/jmab">JMAB project</a>): a Java framework for building macro stock-flow consistent agent-based simulation models. A simulation model is constructed using <a href="http://martinfowler.com/articles/injection.html">dependency injection</a> by creating a <a href="https://blog.mafr.de/2007/11/01/configuration-with-spring-beans/">Spring beans</a> configuration file which specifies which classes to use in the simulation and the values of any attributes (parameters). The Spring configuration file is specified using the system property jabm.config.

## Additional Contents

The folder "paper" contains:
- The pdf document of the paper presenting the model

- All the Plots Produced in the Experiments. In the case of the sensitivity experiments lighter grey lines correspond to higher values of the parameter.

- The Executable Files to run simulations of the model by your own (in both the Baseline Empirical Validation and Policy Experiments Scenarios) and the R Scripts required to transform and analyze the results, and plot the figures. (See Read Me doc. in the “Launch Simulations” Folder)

## Prerequisites

JMAB requires Java version 6 or later.

Note that on Mac OS, you will need to use the Oracle version of Java instead of the default one shipped with the OS.

## Installation

The project archive can be imported directly into the Eclipse IDE as an existing project.

## Running the examples from the Eclipse IDE

The distribution archive can be imported directly into the Eclipse IDE by using the File/Import menu item. Create a launch configuration in the benchmark project with the main class benchmark.Main and specify which configuration file you want to use by setting the system property jabm.config using the JVM argument -D , for example

-Djabm.config=model/mainBaseline.xml

## Documentation

The folder documentation in the <a href="https://github.com/S120/jmab">JMAB project</a> contains a user guide.

The folder paper contains the last working paper version of the model description, plus the data generated by the simulation used for the paper, and the corresponding graphs. It also contains a jar that can be used to run the simulation directly from the console


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
