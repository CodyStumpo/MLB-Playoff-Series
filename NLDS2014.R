# for 2014 NLDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('LAD', 'STL');
bpERAminus = c(105, 100);
wRCplus = c(111,95);
wRCplusL = c(106, 104);
wRCplusR = c(112,93);
NLRunsPerGame = 9600/(162*15); #3.95
parkFactor=c(96,98);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1);
defense = c(9,46)/162;
starters = matrix (c(50,80,85,110,
                     198/27, 202/32, 152/26, 186/32,
                     75,90,95,105,227/32,
                     203/33,107/19,6), nrow=4);#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#LAD = Kershaw*, Greinke, Ryu*, Haren
#STL = Wainwright, Lynn, Wacha, Lackey
starterHand = matrix(c('L','R','L','R',
                       'R','R','R','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
runs2=parks;
for(i in 1:length(runs2)) runs2[i] <- parkFactor[parks[i]]/100 * (
  NLRunsPerGame * (
  (starters[sorder[i],1]/100 * (if (starterHand[sorder[i],1]=='L') wRCplusL[2] else wRCplusR[2])/100 * starters[sorder[i],2]/8.5) 
+ (bpERAminus[1]/100 * wRCplus[2]/100 * (8.5-starters[sorder[i],2])/8.5)
                                         ) - defense[1]);

runs1=runs2;
for(i in 1:length(runs1)) runs1[i] <- parkFactor[parks[i]]/100 * (
  NLRunsPerGame * (
    (starters[sorder[i],3]/100 * (if (starterHand[sorder[i],2]=='L') wRCplusL[1] else wRCplusR[1])/100 * starters[sorder[i],4]/8.5) 
    + (bpERAminus[2]/100 * wRCplus[1]/100 * (8.5-starters[sorder[i],4])/8.5)
  ) - defense[2]);

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
