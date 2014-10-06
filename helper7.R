#called by series-specific files that instantiate the parameters expected here.
pythagExponent=1.83;

runs2=parks;
for(i in 1:length(runs2)) runs2[i] <- parkFactor[parks[i]]/100 * (
  LgRunsPerGame * (
  (starters[sorder[i],1]/100 * (if (starterHand[sorder[i],1]=='L') wRCplusL[2] else wRCplusR[2])/100 * starters[sorder[i],2]/8.5) 
+ (bpERAminus[1]/100 * wRCplus[2]/100 * (8.5-starters[sorder[i],2])/8.5)
                                         ) - defense[1]+baserunning[2]);

runs1=runs2;
for(i in 1:length(runs1)) runs1[i] <- parkFactor[parks[i]]/100 * (
  LgRunsPerGame * (
    (starters[sorder[i],3]/100 * (if (starterHand[sorder[i],2]=='L') wRCplusL[1] else wRCplusR[1])/100 * starters[sorder[i],4]/8.5) 
    + (bpERAminus[2]/100 * wRCplus[1]/100 * (8.5-starters[sorder[i],4])/8.5)
  ) - defense[2]+baserunning[1]);

wPct = runs1^pythagExponent / (runs1^pythagExponent + runs2^pythagExponent); #chance of team1 winning each game

#put in transition matrix, raise it to the 7th power, observe 1st row.

states = c('0-0','1-0','0-1','1-1','2-0','0-2','2-1','1-2','3-0','0-3','2-2','3-1','1-3','4-0','0-4','3-2','2-3','4-1','1-4','3-3','4-2','2-4','4-3','3-4');
transitions=matrix(rep(0,length(states)^2), nrow=length(states), dimnames=list(states, states));
transitions['4-0','4-0']=1;
transitions['4-1','4-1']=1;
transitions['4-2','4-2']=1;
transitions['4-3','4-3']=1;
transitions['0-4','0-4']=1;
transitions['1-4','1-4']=1;
transitions['2-4','2-4']=1;
transitions['3-4','3-4']=1;
transitions['0-0', '1-0']=wPct[1];
transitions['0-0', '0-1']=1-wPct[1];
transitions['1-0', '2-0']=wPct[2];
transitions['1-0', '1-1']=1-wPct[2];
transitions['0-1', '0-2']=1-wPct[2];
transitions['0-1', '1-1']=wPct[2];
transitions['2-0', '3-0']=wPct[3];
transitions['2-0', '2-1']=1-wPct[3];
transitions['0-2', '0-3']=1-wPct[3];
transitions['0-2', '1-2']=wPct[3];
transitions['1-1', '2-1']=wPct[3];
transitions['1-1', '1-2']=1-wPct[3];
transitions['2-1', '3-1']=wPct[4];
transitions['2-1', '2-2']=1-wPct[4];
transitions['1-2', '2-2']=wPct[4];
transitions['1-2', '1-3']=1-wPct[4];
transitions['3-0', '4-0']=wPct[4];
transitions['3-0', '3-1']=1-wPct[4];
transitions['0-3', '1-3']=wPct[4];
transitions['0-3', '0-4']=1-wPct[4];
transitions['2-2', '2-3']=1-wPct[5];
transitions['2-2', '3-2']=wPct[5];
transitions['3-1', '3-2']=1-wPct[5];
transitions['3-1', '4-1']=wPct[5];
transitions['1-3', '2-3']=1-wPct[5];
transitions['1-3', '1-4']=wPct[5];
transitions['3-2', '4-2']=wPct[6];
transitions['3-2', '3-3']=1-wPct[6];
transitions['2-3', '2-4']=1-wPct[6];
transitions['2-3', '3-3']=wPct[6];
transitions['3-3', '3-4']=1-wPct[7];
transitions['3-3', '4-3']=wPct[7];

endstate = transitions %*% transitions %*% transitions %*% transitions %*% transitions %*% transitions %*% transitions;
#expm package may have %^%
