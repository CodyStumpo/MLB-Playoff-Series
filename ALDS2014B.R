# for 2014 ALDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('LAA', 'KCR'); # put hometeam advantage team first
bpERAminus = c(96,89);
wRCplus = c(110,94);
wRCplusL = c(120,100);
wRCplusR = c(107,92);
ALRunsPerGame = 10161/(162*15); #4.18
parkFactor=c(95,101);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1);
defense = c(5,50)/162;
baserunning=c(-1,1)/162;
#should I guess add baserunning, but such a small impact
starters = matrix (c(109,117,89,114,
                     213/34, 175/31, 121/20, 117/24,
                     102,95,90,93,
                     187/30,141/25,227/34,181/30), nrow=4);
#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#LAA = Weaver, Wilson*, Shoemaker, Santiago*
#KCR = Vargas, Duffy*,  Shields, Ventura
starterHand = matrix(c('R','L','R','L',
                       'R','L','R','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
runs2=parks;
for(i in 1:length(runs2)) runs2[i] <- parkFactor[parks[i]]/100 * (
  ALRunsPerGame * (
  (starters[sorder[i],1]/100 * (if (starterHand[sorder[i],1]=='L') wRCplusL[2] else wRCplusR[2])/100 * starters[sorder[i],2]/8.5) 
+ (bpERAminus[1]/100 * wRCplus[2]/100 * (8.5-starters[sorder[i],2])/8.5)
                                         ) - defense[1]+baserunning[2]);

runs1=runs2;
for(i in 1:length(runs1)) runs1[i] <- parkFactor[parks[i]]/100 * (
  ALRunsPerGame * (
    (starters[sorder[i],3]/100 * (if (starterHand[sorder[i],2]=='L') wRCplusL[1] else wRCplusR[1])/100 * starters[sorder[i],4]/8.5) 
    + (bpERAminus[2]/100 * wRCplus[1]/100 * (8.5-starters[sorder[i],4])/8.5)
  ) - defense[2]+baserunning[1]);

wPct = runs1^2 / (runs1^2 + runs2^2); #chance of team1 winning each game

#put in transition matrix, raise it to the 5th power, observe 1st row.
states =c('0-0','1-0','0-1','1-1','2-0','0-2','2-1','1-2','3-0','0-3','2-2','3-1','1-3','3-2','2-3');
transitions=matrix(rep(0,length(states)^2), nrow=length(states), dimnames=list(states, states));
transitions['3-0','3-0']=1;
transitions['3-1','3-1']=1;
transitions['3-2','3-2']=1;
transitions['0-3','0-3']=1;
transitions['1-3','1-3']=1;
transitions['2-3','2-3']=1;
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
transitions['2-2', '2-3']=1-wPct[5];
transitions['2-2', '3-2']=wPct[5];

endstate = transitions %*% transitions %*% transitions %*% transitions %*% transitions;
#expm package may have %^%

tail(endstate['0-0',],7)
endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2']    
