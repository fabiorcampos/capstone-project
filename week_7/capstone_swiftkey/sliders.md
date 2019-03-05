A Word Prediction Algorithm
========================================================
author: Fábio R. Campos
date: 05/03/2019
autosize: true

Data science Capstone Project - Johns Hopkins Specialization

Introduction
========================================================

Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and a whole range of other activities. But typing on mobile devices can be a serious pain.

This application is a predictive model that can be used to predict next word based upon the users textś typing. The probabilistic-language-modeling-based approach 'knows' how certain words tend to be combined together in our language and tends to exhibit a far greater accuracy.


Some Observations
========================================================

The probabilities were all previously computed and are loaded before execution. The app searches through millions of words down the tables to instantly recover the most likely next word.

The algorithm is handles many contractions used in Internet language: e.g. “2 b or not 2” will be translated as “to be or not to” and “be” will be suggested as the next word.

Profanities and bleeped words (e.g. 'f***' and 'f#@%') are removed from user input as were also previously removed from the tables.

Stopwords, on the other hand, were left in, as they are present in normal language and could be the expected next input from a user.


The Algorithm
========================================================

It is based on the widely used 4-gram language model and on the “Stupid Backoff” approach. More details can be found here.

If the user enters e.g. “to be or not to”, it is chopped to the last 3 words (“or not to”).A search is done for a match to the chopped input.If a match is found, the algorith skips to Step 4. Otherwise, the user input is shortened again (“not to”, etc.), and the algorithm go back to Step 2. If a match was found, it is returned to the user interface. Otherwise, the most frequent word in the corpora is returned.

The Shiny App
========================================================

The goal of this project was to create a product to highlight the prediction algorithm built and to provide an interface that can be easily accessed by others.

The working app is available here: [Application](https://fabiorcampos.shinyapps.io/datascience_project/)


