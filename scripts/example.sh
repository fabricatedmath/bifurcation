#!/bin/bash

bifurcation-exe -F "sin(2*sin(0.02*t)*y - 3*cos(0.03*t)*x)*exp(-abs (sin(0.11*t)*sin (3*x+1-2*y) - sin(0.19*t)*cos(x-3*cos(0.23*t)*y+1)))" -G "cos(2*sin(0.07*t)*y - 3*cos(0.05*t)*x)*exp(-abs (cos(0.13*t)*cos (3*x+1-2*y) - sin(0.17*t)*cos(x-3*sin(0.21*t)*y+1)))" -H 10
