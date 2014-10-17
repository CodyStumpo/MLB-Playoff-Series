# for 2014 ALCS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('STL', 'SFG'); # put hometeam advantage team first
bpERAminus = c(100,95);
wRCplus = c(95,101);
wRCplusL = c(104,104);
wRCplusR = c(93,99);
LgRunsPerGame = 9600/(162*15); #3.95
parkFactor=c(98,93);
parks = c(1,1,2,2,2,1,1);
sorder=c(1,2,3,4,1,2,3);
defense = c(46,-1)/162;
baserunning=c(-11,-2)/162;
starters = matrix (c(75,90,97,105,
                     227/32,203/33, 198/31, 180/31, 
                     86,106, 102, 112,
                     217/33,202/32,189/31,184/32), nrow=4);
                     
#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#STL = Wainwright, Lynn, Lackey, Miller
#Had Wacha at 95, 107/19
# SFG Bumgarner*, Peavy, Hudson, Vogelsong

starterHand = matrix(c('R','R','R','R',
                       'L','R','R','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
source(file="helper7.R");

round(wPct,2)
round(tail(endstate['0-0',],11),2)
round(endstate['0-0','4-0']  + endstate['0-0','4-1']+ endstate['0-0','4-2']+ endstate['0-0','4-3'],2) 

