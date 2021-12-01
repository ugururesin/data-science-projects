# =============================================================================
# PYTHON PROBABILITY CODES
# =============================================================================

import numpy as np
np.random.randint(2)            #gives 0 or 1 (2 is exclusive)
np.random.randint(2, size=5)    #gives 5 values 0 or 1
np.random.randint(5,10)         #gives a value between 5 and 9
np.random.choice([0, 1], p=[0.6, 0.4]) #biased coin (60% for 0)
#
# Let O: HEAD, 1: TAIL
# Simulate 1e6 times a coin and find the probability of 2 HEADS 
simulate = np.random.randint(2, size=(int(1e6), 2)) #1e6 times random arrays
simulate.sum(axis=1)                                #sum the arrays
(simulate == 0).mean()                              #sum of two heads = 0
#
#
# Three bias toss (P(H)=0.6), check the outcome of 1 HEAD (sum=2)
tests = np.random.choice([0, 1], size=(int(1e6), 3), p=[0.6, 0.4])
test_sums = tests.sum(axis=1)   # sums of all tests
(test_sums == 2).mean()         # proportion of tests in which one head occurs
#
# A DIE ROLLS an EVEN NUMBER
tests = np.random.choice(np.arange(1, 7), size=int(1e6))
(tests % 2 == 0).mean()     # proportion of tests that produced an even number
#
# TWO DICE ROLL a DOUBLE
first = np.random.choice(np.arange(6), size=int(1e6))
second = np.random.choice(np.arange(6), size=int(1e6))
# proportion of tests where the 1st and 2nd die rolled the same number
(first == second).mean()


## BINOMIAL DISTRIBUTION
# number of heads from 10 fair coin flips
np.random.binomial(10, 0.5)
# results from 20 tests with 10 coin flips
np.random.binomial(10, 0.5, 20)
# mean number of heads from the 20 tests
np.random.binomial(10, 0.5, 20).mean()
# reflects the fairness of the coin more closely as # tests increases
np.random.binomial(10, 0.5, 1000000).mean()
import matplotlib.pyplot as plt
plt.hist(np.random.binomial(10, 0.5, 1000000));
# gets more narrow as number of flips increase per test
plt.hist(np.random.binomial(100, 0.5, 1000000));


## BAYES RULE
# load dataset
import pandas as pd
df = pd.read_csv('cancer_test_data.csv')
df.head()
#
# What proportion of patients who tested positive has cancer?
df.query('test_result == "Positive"')['has_cancer'].mean()
# What proportion of patients who tested positive doesn't have cancer?
1 - df.query('test_result == "Positive"')['has_cancer'].mean()
# What proportion of patients who tested negative has cancer?
df.query('test_result == "Negative"')['has_cancer'].mean()
# What proportion of patients who tested negative doesn't have cancer?
1 - df.query('test_result == "Negative"')['has_cancer'].mean()


