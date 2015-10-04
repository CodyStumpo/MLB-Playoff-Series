# for 2014 ALDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('BAL', 'DET'); # put hometeam advantage team first
bpERAminus = c(87,108);
wRCplus = c(104,111);
wRCplusL = c(106,120);
wRCplusR = c(104,108);
LgRunsPerGame = 10161/(162*15); #4.18
parkFactor=c(102,102);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1);
defense = c(52,-55)/162;
baserunning=c(-6,0)/162;
starters = matrix (c(100,96,110,102,
                     207/34, 185/31, 165/28, 186/32,
                     80,105,80,93,
                     220/33,206/32,248/34,202/31), nrow=4);
#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#BAL = Tillman, Chen*, Gonzalez, Norris
#DET = Scherzer, Verlander, Price*, Porcello
starterHand = matrix(c('R','L','R','R',
                       'R','R','L','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
source(file="helper.R");

round(wPct,2)
round(tail(endstate['0-0',],7),2)
round(endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2'],2) 

