# for 2014 NLDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('WAS', 'SFG');
bpERAminus = c(87, 95);
wRCplus = c(99,101);
wRCplusL = c(104, 104);
wRCplusR = c(98,99);
LgRunsPerGame = 9600/(162*15); #3.95
parkFactor=c(100,93);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1);
defense = c(2,-1)/162;
baserunning=c(13,-2)/162
starters = matrix (c(78,76,92,92,
                     215/34, 200/32, 164/25, 199/31,
                     106,112,86,102,
                     202/32,184/32,217/33,189/31), nrow=4);#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#WAS = Strasburg, Zimmerman, Fister, Roark?  Gonzalez* would be 90, 159/27
#SFG = Peavy, Vogelsong, Bumgarner*, Hudson
#Had Wacha at 95, 107/19
starterHand = matrix(c('R','R','R','R',
                       'R','R','L','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4

source(file="helper.R");

round(tail(endstate['0-0',],7),2)
round(endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2'],2) 
