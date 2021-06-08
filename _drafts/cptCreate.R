#General functions for tables
prior.CPT <- function(node, state1, state2, prob1){
  states <- c(state1, state2)
  dimnames <- list(states)
  names(dimnames) <- node 
  array(c(prob1, 1-prob1), dim = 2, dimnames = dimnames)
}

#with default values and no dots:D
priorCPT <- function(node, state1 = 1, state2 = 0, prob1){
  states <- c(state1, state2)
  dimnames <- list(states)
  names(dimnames) <- node 
  array(c(prob1, 1-prob1), dim = 2, dimnames = dimnames)
}





single.CPT <- function(eNode, hNode,
                       eState1, eState2,
                       h1, h2,
                       probH1E, probH2E){
  eStates <- c(eState1,eState2)
  hStates <- c(h1,h2)
  dimnames <- list(eStates,hStates)
  names(dimnames) <- c(eNode,hNode)
  array(
    c(probH1E, as.numeric(1-probH1E),
      probH2E,as.numeric(1-probH2E)), 
    dim = c(2,2),
    dimnames = dimnames
  )
}


#with defaults and no dots
singleCPT <- function(eNode, hNode,
                      eState1 = "1", eState2 = "0",
                      hState1 = "1", hState2 = "0",
                      probEifHS1, probEifHS2){
  eStates <- c(eState1,eState2)
  hStates <- c(hState1,hState2)
  dimnames <- list(eStates,hStates)
  names(dimnames) <- c(eNode,hNode)
  array(
    c(probEifHS1, as.numeric(1-probEifHS1),
      probEifHS2,as.numeric(1-probEifHS2)), 
    dim = c(2,2),
    dimnames = dimnames
  )
}



doubleCPT <- function(eNode, h1Node, h2Node, eState1 = "1" , eState2 = "0", h1State1 = "1", h1State2 = "0", h2State1 = "1", h2State2 = "0", probEifH1S1H2S1, probEifH1S1H2S2, probEifH1S2H2S1,probEifH1S2H2S2){
  eStates <- c(eState1,eState2)
  h1States <- c(h1State1,h2State2)
  h2States <- c(h2State1,h2State2)
  dimnames <- list(eStates,h1States,h2States)
  names(dimnames) <- c(eNode,h1Node,h2Node)
  array(c(
    probEifH1S1H2S1,as.numeric(1-probEifH1S1H2S1),
    probEifH1S2H2S1,as.numeric(1-probEifH1S2H2S1),
    probEifH1S1H2S2,as.numeric(1-probEifH1S1H2S2),
    probEifH1S2H2S2,as.numeric(1-probEifH1S2H2S2)
  ),
  dim = c(2,2,2), dimnames = dimnames)
}


# example for testing and clarity
#eProb2 <- doubleCPT(eNode = "eNode", h1Node = "h1Node", h2Node = "h2Node", probEifH1S1H2S1 = .48,probEifH1S2H2S1 = .56,probEifH1S1H2S2 =.58,probEifH1S2H2S2 = .7)



