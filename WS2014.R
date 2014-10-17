# for 2014 WS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com

# WS uses run environment of the league of the park the game is played at.
# Should I adapt wRC+ also?
# if rules == A, KC offense as normal but SFG offense NP
# if rules == N, SFG offense as normal but KC offense worse?
# for now, no. wRC+ is already to MLB non-pitchers (or AL/NL NP but those are pretty much the same)
# so assume marginal DH is an average bat for the team. Not that the person being the DH is average, 
# but that the lineup with/without the DH involves one extra additional average hitter

teams = c('KCR','SFG'); # put hometeam advantage team first
bpERAminus = c(89,95);
wRCplus = c(94,101);
wRCplusNP = c(94,107)
wRCplusL = c(100,104);
wRCplusLNP = c(100,109);
wRCplusR = c(92,99);
wRCplusRNP = c(92,106);
ALLgRunsPerGame = 10161/(162*15); #4.18
NLLgRunsPerGame = 9600/(162*15); #3.95
LgRunsPerGane = c(ALLgRunsPerGame, NLLgRunsPerGame);
parkFactor=c(101,93);
parks = c(1,1,2,2,2,1,1);
sorder=c(1,2,3,4,1,2,3);
defense = c(50,-1)/162;
baserunning=c(1,-2)/162;
starters = matrix (c(90,93,101,111,
                     227/34,202/32,187/30,181/30,
                     86,106, 102, 112,
                     217/33,202/32,189/31,184/32), nrow=4);
#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4

# KCR  Shields 90, Ventura 93,Vargas* 101, Guthrie 111?
#KCR - Duffy* = 95, 141/25
#KCR - Guthrie = 111, 202/32
# SFG Bumgarner*, Peavy, Hudson, Vogelsong

starterHand = matrix(c(
                       'R','R','L','R',
                       'L','R','R','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4

#called by series-specific files that instantiate the parameters expected here.
pythagExponent=1.83;

runs2=parks;
for(i in 1:length(runs2)) runs2[i] <- parkFactor[parks[i]]/100 * (
  LgRunsPerGane[parks[i]] * (
    (starters[sorder[i],1]/100 * (if (starterHand[sorder[i],1]=='L') wRCplusL[2] else wRCplusR[2])/100 * starters[sorder[i],2]/8.5) 
    + (bpERAminus[1]/100 * wRCplus[2]/100 * (8.5-starters[sorder[i],2])/8.5)
  ) - defense[1]+baserunning[2]);

runs1=runs2;
for(i in 1:length(runs1)) runs1[i] <- parkFactor[parks[i]]/100 * (
  LgRunsPerGane[parks[i]] * (
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


round(wPct,2)
round(tail(endstate['0-0',],11),2)
round(endstate['0-0','4-0']  + endstate['0-0','4-1']+ endstate['0-0','4-2']+ endstate['0-0','4-3'],2) 

