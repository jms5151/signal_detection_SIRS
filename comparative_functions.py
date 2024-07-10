# -*- coding: utf-8 -*-
"""
Created on Wed May 29 09:11:57 2024

@author: jamie
"""

import numpy as np
import pandas as pd
# from scipy.optimize import curve_fit
# from sympy import symbols, diff

# Function definitions

def linear(x, a, b):
    return a * x + b

def quadratic(x, a, b, c):
    return a * x**2 + b * x + c

def modified_briere(x, a, b, t0):
    y = np.where(x >= t0, a * x * (x - t0) * np.sqrt(b - x), 0)
    return y

def michaelis_menten(x, Vmax, Km):
    return (Vmax * x) / (Km + x)

def exponential_growth(x, a, b):
    return a * np.exp(b * x)

def exponential_decay(x, a, b):
    return a * np.exp(-b * x)

def logistic_growth(x, L, k, x0):
    return L / (1 + np.exp(-k * (x - x0)))

# Function to generate y-values given a function type and parameters
def generate_function(func, x_range, params):
    x = np.linspace(x_range[0], x_range[1], 500)
    y = func(x, *params)
    return x, y

# Normalize y-values to fall within the specified y range (0 to 1.25)
def normalize_y(y, y_range=(0, 1.25)):
    y_min, y_max = np.min(y), np.max(y)
    y_normalized = (y - y_min) / (y_max - y_min) * (y_range[1] - y_range[0]) + y_range[0]
    return y_normalized

from plotnine import ggplot, aes, geom_line, ggtitle, xlab, ylab, theme_bw

def plot_function_ggplot(x, y, title):
    # Convert x and y to a DataFrame for plotnine
    df = pd.DataFrame({'x': x, 'y': y})
    
    # Create the plot using plotnine
    plot = (ggplot(df, aes(x='x', y='y')) +
            geom_line() +
            ggtitle(title) +
            xlab('x') +
            ylab('y') +
            theme_bw())
    print(plot)


# Generate and plot functions within the specified x and y ranges
x_range = (1, 20)

# Linear Function
params = (0.1, 0)  # a, b
x, y = generate_function(linear, x_range, params)
y_normalized = normalize_y(y)
plot_function_ggplot(x, y_normalized, "Linear Function: y = 0.1x")

# Quadratic Function
params = (0.01, -0.2, 1)  # a, b, c
x, y = generate_function(quadratic, x_range, params)
y_normalized = normalize_y(y)
plot_function_ggplot(x, y_normalized, "Quadratic Function: y = 0.01x^2 - 0.2x + 1")

# Modified Briere Function
params = (0.0002, 20, 5)  # a, b, t0
x, y = generate_function(modified_briere, x_range, params)
y_normalized = normalize_y(y)
plot_function_ggplot(x, y_normalized, "Modified Briere Function")

# Michaelis-Menten Function
params = (1.5, 5)  # Vmax, Km
x, y = generate_function(michaelis_menten, x_range, params)
y_normalized = normalize_y(y)
plot_function_ggplot(x, y_normalized, "Michaelis-Menten Function")

# Exponential Growth Function
params = (1, 0.3)  # a, b
x, y = generate_function(exponential_growth, x_range, params)
y_normalized = normalize_y(y)
plot_function_ggplot(x, y_normalized, "Exponential Growth Function")

# Exponential Decay Function
params = (1, 0.3)  # a, b
x, y = generate_function(exponential_decay, x_range, params)
y_normalized = normalize_y(y)
plot_function_ggplot(x, y_normalized, "Exponential Decay Function")

# Logistic Growth Function
params = (1, 1, 10)  # L, k, x0
x, y = generate_function(logistic_growth, x_range, params)
y_normalized = normalize_y(y)
plot_function_ggplot(x, y_normalized, "Logistic Growth Function")

