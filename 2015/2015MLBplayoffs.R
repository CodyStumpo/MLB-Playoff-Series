# for 2015 DS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com

source(file="teams.R")


fiveGameSeries<- function(twoTeams){

h=twoTeams[1]
a=twoTeams[2]
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1); #should let this vary by team

LgRunsPerGame = teams[c(h,a),]$LgRunsPerGame

bpERAminus = teams[c(h,a),]$bpERAminus 
wRCplus = teams[c(h,a),]$wRCplus 
wRCplusL =teams[c(h,a),]$wRCplusL 
wRCplusR = teams[c(h,a),]$wRCplusR 
parkFactor=teams[c(h,a),]$parkFactor 
defense = teams[c(h,a),]$defense 
baserunning=teams[c(h,a),]$baserunning 


starters = matrix (c(teams[h,]$starter1ERAminus, teams[h,]$starter2ERAminus, teams[h,]$starter3ERAminus,teams[h,]$starter4ERAminus,
                     teams[h,]$starter1IP, teams[h,]$starter2IP, teams[h,]$starter3IP,teams[h,]$starter4IP,
                     teams[a,]$starter1ERAminus, teams[a,]$starter2ERAminus, teams[a,]$starter3ERAminus,teams[a,]$starter4ERAminus,
                     teams[a,]$starter1IP, teams[a,]$starter2IP, teams[a,]$starter3IP,teams[a,]$starter4IP), 
                   nrow=4);



starterHand = matrix(c(teams[h,]$starter1Hand, teams[h,]$starter2Hand, teams[h,]$starter3Hand,teams[h,]$starter4Hand,
                       teams[a,]$starter1Hand, teams[a,]$starter2Hand, teams[a,]$starter3Hand,teams[a,]$starter4Hand), 
                     nrow=4);


#source(file="helper.R");
pythagExponent=1.83;

runs2=parks;
for(i in 1:length(runs2)) runs2[i] <- parkFactor[parks[i]]/100 * (
  LgRunsPerGame[parks[i]] * (
    (starters[sorder[i],1]/100 * (if (starterHand[sorder[i],1]=='L') wRCplusL[2] else wRCplusR[2])/100 * starters[sorder[i],2]/8.5) 
    + (bpERAminus[1]/100 * wRCplus[2]/100 * (8.5-starters[sorder[i],2])/8.5)
  ) - defense[1]+baserunning[2]);

runs1=runs2;
for(i in 1:length(runs1)) runs1[i] <- parkFactor[parks[i]]/100 * (
  LgRunsPerGame[parks[i]] * (
    (starters[sorder[i],3]/100 * (if (starterHand[sorder[i],2]=='L') wRCplusL[1] else wRCplusR[1])/100 * starters[sorder[i],4]/8.5) 
    + (bpERAminus[2]/100 * wRCplus[1]/100 * (8.5-starters[sorder[i],4])/8.5)
  ) - defense[2]+baserunning[1]);

wPct = runs1^pythagExponent / (runs1^pythagExponent + runs2^pythagExponent); #chance of team1 winning each game

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



x=round(wPct,2)
y=round(tail(endstate['0-0',],7),2)
z=round(endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2'],2) 
list(x,y,z)
}

sevenGameSeries<- function(twoTeams){
  
  h=twoTeams[1]
  a=twoTeams[2]
  parks = c(1,1,2,2,2,1,1);
  sorder= c(1,2,3,4,1,2,3);
  
  LgRunsPerGame = teams[c(h,a),]$LgRunsPerGame
  
  bpERAminus = teams[c(h,a),]$bpERAminus 
  wRCplus = teams[c(h,a),]$wRCplus 
  wRCplusL =teams[c(h,a),]$wRCplusL 
  wRCplusR = teams[c(h,a),]$wRCplusR 
  parkFactor=teams[c(h,a),]$parkFactor 
  defense = teams[c(h,a),]$defense 
  baserunning=teams[c(h,a),]$baserunning 
  
  
  starters = matrix (c(teams[h,]$starter1ERAminus, teams[h,]$starter2ERAminus, teams[h,]$starter3ERAminus,teams[h,]$starter4ERAminus,
                       teams[h,]$starter1IP, teams[h,]$starter2IP, teams[h,]$starter3IP,teams[h,]$starter4IP,
                       teams[a,]$starter1ERAminus, teams[a,]$starter2ERAminus, teams[a,]$starter3ERAminus,teams[a,]$starter4ERAminus,
                       teams[a,]$starter1IP, teams[a,]$starter2IP, teams[a,]$starter3IP,teams[a,]$starter4IP), 
                     nrow=4);
  
  
  
  starterHand = matrix(c(teams[h,]$starter1Hand, teams[h,]$starter2Hand, teams[h,]$starter3Hand,teams[h,]$starter4Hand,
                         teams[a,]$starter1Hand, teams[a,]$starter2Hand, teams[a,]$starter3Hand,teams[a,]$starter4Hand), 
                       nrow=4);
  
  

  pythagExponent=1.83;
  
  runs2=parks;
  for(i in 1:length(runs2)) runs2[i] <- parkFactor[parks[i]]/100 * (
    LgRunsPerGame[parks[i]] * (
      (starters[sorder[i],1]/100 * (if (starterHand[sorder[i],1]=='L') wRCplusL[2] else wRCplusR[2])/100 * starters[sorder[i],2]/8.5) 
      + (bpERAminus[1]/100 * wRCplus[2]/100 * (8.5-starters[sorder[i],2])/8.5)
    ) - defense[1]+baserunning[2]);
  
  runs1=runs2;
  for(i in 1:length(runs1)) runs1[i] <- parkFactor[parks[i]]/100 * (
    LgRunsPerGame[parks[i]] * (
      (starters[sorder[i],3]/100 * (if (starterHand[sorder[i],2]=='L') wRCplusL[1] else wRCplusR[1])/100 * starters[sorder[i],4]/8.5) 
      + (bpERAminus[2]/100 * wRCplus[1]/100 * (8.5-starters[sorder[i],4])/8.5)
    ) - defense[2]+baserunning[1]);
  
  wPct = runs1^pythagExponent / (runs1^pythagExponent + runs2^pythagExponent); #chance of team1 winning each game
  
  #put in transition matrix, raise it to the 5th power, observe 1st row.
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
  
 
  x=round(wPct,2)
  y=round(tail(endstate['0-0',],11),2)
  z=round(endstate['0-0','4-0']  + endstate['0-0','4-1']+ endstate['0-0','4-2']+ endstate['0-0','4-3'],2)  
  list(x,y,z)
}


fiveGameSeries(c('LAD','NYM'))
sevenGameSeries(c('LAD','STL'))


TORTEX5=fiveGameSeries(c('TOR','TEX'))
KCRHOU5=fiveGameSeries(c('KCR','HOU'))

TORKCR7=sevenGameSeries(c('TOR','KCR'))
TORHOU7=sevenGameSeries(c('TOR','HOU'))

KCRTEX7=sevenGameSeries(c('KCR','TEX'))
TEXHOU7=sevenGameSeries(c('TEX','HOU'))


TOR2WS=TORTEX5[3][[1]]*(TORKCR7[3][[1]]*KCRHOU5[3][[1]] + TORHOU7[3][[1]]*(1-KCRHOU5[3][[1]])) #TOR2CS * (Win vs KCR + win vs HOU) 
TEX2WS=(1-TORTEX5[3][[1]])*((1-KCRTEX7[3][[1]])*KCRHOU5[3][[1]] + TEXHOU7[3][[1]]*(1-KCRHOU5[3][[1]])) #TEX2CS * (Win vs KCR + win vs HOU)

KCR2WS=KCRHOU5[3][[1]]*( TORTEX5[3][[1]] * (1-TORKCR7[3][[1]]) + (1-TORTEX5[3][[1]])*KCRTEX7[3][[1]])#KCR2CS * (win vs TOR + win vs TEX)
HOU2WS=(1-KCRHOU5[3][[1]])*( TORTEX5[3][[1]] * (1-TORHOU7[3][[1]]) + (1-TORTEX5[3][[1]])*(1-TEXHOU7[3][[1]]))#HOU2CS * (win vs TOR + win vs TEX)
