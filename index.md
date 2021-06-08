---
title:
layout: page
toc: true
#title: Bayesian Networks for the Legal Probabilism SEP entry
output:
  md_document:
    variant: markdown_github
    preserve_yaml: true
---




Suppose you heard easy money can be made by online trading. You want to give it a shot,  so you open an account with an online broker. You google a bit and apparently one way to go about trading is to define a strategy telling you when to buy and when to sell,  "backtest" it (that is, see how it would have performed on past data), perhaps tweaking it a bit in the process, and to go ahead once you're more or less satisfied. That's the way to beat the market. Or so they say...

Here's on one hand an explanation of how you could formulate and "backtest" your trading strategy using **R**, and on the other, why it might not be worth your time, as backtesting is a flawed methodology. I also show you what methodology is better (hint: it uses Monte Carlo methods to estimate the chance a random strategy would to as good as your shiny source of pride), and why you might be disappointed with the result.

- While I am aware some of the moves could be made more efficient and code shorter, I do certain things in a somewhat lengthy manner for the sake of accessibility.

- For a sensible criticism of subjective technical analysis and a nice treatment of some of the conceptual points I'm going to discuss, I highly recommend [Evidence-Based Technical Analysis](https://www.amazon.com/Evidence-Based-Technical-Analysis-Scientific-Statistical/dp/0470008741) by David Aronson.


**Contents**
* TOC
{:toc}



### Set-up
