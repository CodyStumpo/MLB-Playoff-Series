# for 2015 ALDS, produce run expectancy for each team against projected starter & bullpen, adjust for defense & park
# compute win probability of each game using pythagorean formula
# use Markov process to determine probabilities of endstate of series
# Cody Stumpo, codybass@gmail.com
teams = c('TOR', 'TEX');
bpERAminus = c(87, 101);
wRCplus = c(117,96);
wRCplusL = c(123, 96);
wRCplusR = c(115,96);
LgRunsPerGame = 10600/(162*15); #4.36
parkFactor=c(102,106);
parks = c(1,1,2,2,1);
sorder=c(1,2,3,4,1);
defense = c(10,13)/162;
baserunning=c(10,5)/162
starters = matrix (c(69,107,85,104,
                     220/32,214/33, 27/4,170/28,
                     87,110,118,94,
                     212/32,205/33, 59/10,184/33), nrow=4);#put t1 era 1-4, t1 ip 5-8.
# columns (in result, transposed from entry) are ERA-, IP, for team 1, then for team 2. Rows are starters 1-4
#TOR = Price* (69, 220/32), Dickey (107, 214/33), Stroman (85, 27/4), Estrada (104, 170/28)
#TEX = Hamels* (87, 212/32), Lewis (110, 205/33), Holland* (118,59/10), Gallardo (94, 184/33) or Perez* (95, 79/14)

starterHand = matrix(c('L','R','R','R',
                       'L','R','L','R'), nrow=4);#put t1 starters 1-4, t2 5-8
#columns are team 1, team 2. rows are starters 1-4
source(file="helper.R");

round(wPct,2)
round(tail(endstate['0-0',],7),2)
round(endstate['0-0','3-0']  + endstate['0-0','3-1']+ endstate['0-0','3-2'],2) 

