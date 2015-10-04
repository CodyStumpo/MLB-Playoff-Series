# for 2015 NLDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('LAD', 'NYM');
bpERAminus = c(97, 97);
wRCplus = c(105,100);
wRCplusL = c(111, 103);
wRCplusR = c(104,99);
LgRunsPerGame = 9950/(162*15); #4.09
parkFactor=c(96,95);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1);
defense = c(0,-5)/162;
baserunning=c(-14,8)/162
starters = matrix (c(55,60,100,100,
                     235/33, 223/32, 180/31, 190/32,
                     81,72,83,105,
                     183/28,193/30, 143/23, 190/31), nrow=4);#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#LAD = Kershaw*, Greinke, Anderson*, Wood*
#STL = Harvey, deGrom, Syndergaard, Colon

starterHand = matrix(c('L','R','L','L',
                       'R','R','R','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
source(file="helper.R");

round(wPct,2)
round(tail(endstate['0-0',],7),2)
round(endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2'],2) 

