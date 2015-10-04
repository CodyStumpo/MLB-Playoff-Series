# for 2014 ALCS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('BAL', 'KCR'); # put hometeam advantage team first
bpERAminus = c(87,89);
wRCplus = c(104,94);
wRCplusL = c(106,100);
wRCplusR = c(104,92);
LgRunsPerGame = 10161/(162*15); #4.18
parkFactor=c(102,101);
parks = c(1,1,2,2,2,1,1);
sorder=c(1,2,3,4,1,2,3);
defense = c(52,50)/162;
baserunning=c(-6,1)/162;
starters = matrix (c(100,96,102,110,
                     207/34, 185/31, 186/32,165/28, 
                     101,93,90,111,
                     187/30,202/32,227/34,181/30), nrow=4);
#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#BAL = Tillman, Chen*, Gonzalez, Norris
# KCR Vargas* 101, Ventura 93, Shields 90, Guthrie 111?
#KCR - Duffy* = 95, 141/25
#KCR - Guthrie = 111, 202/32
starterHand = matrix(c('R','L','R','R',
                       'L','R','R','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
source(file="helper7.R");

round(wPct,2)
round(tail(endstate['0-0',],11),2)
round(endstate['0-0','4-0']  + endstate['0-0','4-1']+ endstate['0-0','4-2']+ endstate['0-0','4-3'],2) 

