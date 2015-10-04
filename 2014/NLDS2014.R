# for 2014 NLDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('LAD', 'STL');
bpERAminus = c(105, 100);
wRCplus = c(111,95);
wRCplusL = c(106, 104);
wRCplusR = c(112,93);
LgRunsPerGame = 9600/(162*15); #3.95
parkFactor=c(96,98);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1);
defense = c(9,46)/162;
baserunning=c(2,-11)/162
starters = matrix (c(50,80,85,110,
                     198/27, 202/32, 152/26, 186/32,
                     75,90,97,105,
                     227/32,203/33, 198/31, 180/31), nrow=4);#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#LAD = Kershaw*, Greinke, Ryu*, Haren
#STL = Wainwright, Lynn, Lackey, Miller
#Had Wacha at 95, 107/19
starterHand = matrix(c('L','R','L','R',
                       'R','R','R','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
source(file="helper.R");

round(wPct,2)
round(tail(endstate['0-0',],7),2)
round(endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2'],2) 

