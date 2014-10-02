# for 2014 ALDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('LAA', 'KCR'); # put hometeam advantage team first
bpERAminus = c(96,89);
wRCplus = c(110,94);
wRCplusL = c(120,100);
wRCplusR = c(107,92);
LgRunsPerGame = 10161/(162*15); #4.18
parkFactor=c(95,101);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,5);
defense = c(5,50)/162;
baserunning=c(-1,1)/162;
#should I guess add baserunning, but such a small impact
starters = matrix (c(109,89,117, 114,93,
                     213/34, 121/20, 175/31,  (213/34)-.5, (121/20)-.5,
                     102,111,90,93,107,
                     187/30,202/32,227/34,181/30, (187/30)-.5), nrow=5);
#put t1 era 1-5, t1 ip 6-10.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4



# Weaver, Shoemaker, Wilson*, Weaver  
#  Vargas, Ventura, Shields, Guthrie?
#KCR - Duffy* = 95, 141/25
#LAA - Santiago* = 114, 117/24
#KCR - Guthrie = 111, 202/32
starterHand = matrix(c('R','R','L','R', 'R',
                       'R','R','R','R', 'R'), nrow=5);#put t1 starters 1-5, t2 6-10
#columns are team 1, team 2. rows are starters 1-5
source(file="helper.R");

round(tail(endstate['0-0',],7),2)
round(endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2'],2) 

