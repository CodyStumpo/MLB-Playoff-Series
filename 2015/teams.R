#load up data for 2015 playoff teams (post WC games) so can efficiently run series
#generally, use avg (ERA-, FIP-, xFIP-) as estimator of pitching
#generally, use avg(UZR, DRS) as estimator of defense

options(stringsAsFactors = FALSE)

teams=data.frame(name=character(), 
                 bpERAminus=double(),
                 wRCplus=double(),
                 wRCplusL=double(),
                 wRCplusR=double(),
                 LgRunsPerGame=double(),
                 parkFactor=double(),
                 defense=double(),
                 baserunning=double(),
                 starter1ERAminus=double(),
                 starter1IP=double(),
                 starter1Hand=character(),
                 starter2ERAminus=double(),
                 starter2IP=double(),
                 starter2Hand=character(),
                 starter3ERAminus=double(),
                 starter3IP=double(),
                 starter3Hand=character(),
                 starter4ERAminus=double(),
                 starter4IP=double(),
                 starter4Hand=character(),
                 stringsAsFactors = FALSE)

TOR = data.frame(name='TOR', 
        bpERAminus=88, 
        wRCplus=117,
        wRCplusL=123,
        wRCplusR=115,
        LgRunsPerGame=10651/(162*15),
        parkFactor=102,
        defense=10/162,
        baserunning=10/162,
        starter1ERAminus=69,#Price
        starter1IP=220/32,
        starter1Hand='L',
        starter2ERAminus=107,#Dickey
        starter2IP=214/33,
        starter2Hand='R',
        starter3ERAminus=85,#Stroman
        starter3IP=27/4,
        starter3Hand='R',
        starter4ERAminus=104,#Estrada
        starter4IP=170/28,
        starter4Hand='R'
)
      

teams =rbind(teams, TOR)


TEX = data.frame(name='TEX', 
                 bpERAminus=101, 
                 wRCplus=96,
                 wRCplusL=96,
                 wRCplusR=96,
                 LgRunsPerGame=10651/(162*15),
                 parkFactor=106,
                 defense=13/162,
                 baserunning=5/162,
                 starter2ERAminus=87, #Hamels
                 starter2IP=212/32,
                 starter2Hand='L',
                 starter3ERAminus=110, #Lewis
                 starter3IP=205/33,
                 starter3Hand='R',
                 starter4ERAminus=118, #Holland
                 starter4IP=59/10,
                 starter4Hand='L',
                 starter1ERAminus=94,#Gallardo
                 starter1IP=184/33,
                 starter1Hand='R'
)#Perez (95, 79/14, 'L')

teams =rbind(teams, TEX)


LAD = data.frame(name='LAD', 
                 bpERAminus=97, 
                 wRCplus=105,
                 wRCplusL=111,
                 wRCplusR=104,
                 LgRunsPerGame=9996/(162*15),
                 parkFactor=96,
                 defense=0/162,
                 baserunning=-14/162,
                 starter1ERAminus=55, #Kershaw
                 starter1IP=233/33,
                 starter1Hand='L',
                 starter2ERAminus=60, #Greinke
                 starter2IP=223/32,
                 starter2Hand='R',
                 starter3ERAminus=100, #Anderson
                 starter3IP=180/31,
                 starter3Hand='L',
                 starter4ERAminus=100,#Wood
                 starter4IP=190/32,
                 starter4Hand='L'
)

teams =rbind(teams, LAD)


NYM = data.frame(name='NYM', 
                 bpERAminus=97, 
                 wRCplus=100,
                 wRCplusL=103,
                 wRCplusR=99,
                 LgRunsPerGame=9996/(162*15),
                 parkFactor=95,
                 defense=-5/162,
                 baserunning=8/162,
                 starter1ERAminus=72, #deGrom
                 starter1IP=193/30,
                 starter1Hand='R',
                 starter2ERAminus=83, #Syndergaard
                 starter2IP=143/23,
                 starter2Hand='R',
                 starter3ERAminus=81, #Harvey
                 starter3IP=183/28,
                 starter3Hand='R',
                 starter4ERAminus=80,#Matz
                 starter4IP=36/6,
                 starter4Hand='L'
)#Colon (105, 190/31, R)

teams =rbind(teams, NYM)

STL = data.frame(name='STL', 
                 bpERAminus=88, 
                 wRCplus=96,
                 wRCplusL=83,
                 wRCplusR=102,
                 LgRunsPerGame=9996/(162*15),
                 parkFactor=98,
                 defense=15/162,
                 baserunning=-5/162,
                 starter1ERAminus=76, #Garcia
                 starter1IP=130/20,
                 starter1Hand='L',
                 starter2ERAminus=87, # Lackey
                 starter2IP=218/33,
                 starter2Hand='R',
                 starter3ERAminus=90, #Lynn
                 starter3IP=175/31,
                 starter3Hand='R',
                 starter4ERAminus=96,#Wacha
                 starter4IP=181/30,
                 starter4Hand='L'
)
#Garcia (76, 130/20, 'L')
#Lackey (87, 218/33, 'R')
#Lynn (90, 175/31, 'R')
#Wacha (96,181/30, 'R')


teams =rbind(teams, STL)


KCR = data.frame(name='KCR', 
                 bpERAminus=84, 
                 wRCplus=99,
                 wRCplusL=98,
                 wRCplusR=100,
                 LgRunsPerGame=10651/(162*15),
                 parkFactor=101,
                 defense=57/162,
                 baserunning=-1/162,
                 starter1ERAminus=93, #Ventura
                 starter1IP=163/28,
                 starter1Hand='R',
                 starter2ERAminus=90, #Cueto
                 starter2IP=81/13,
                 starter2Hand='R',
                 starter3ERAminus=96, #Volquez
                 starter3IP=198/33,
                 starter3Hand='R',
                 starter4ERAminus=112,#Medlen
                 starter4IP=44/8,
                 starter4Hand='R'
)
#Ventura (93 163/28,'R')
#Cueto (90, 81/13,'R')
#volquez (96 ,198/33,'R')
#medlen  (112, 44/8,'R')

teams =rbind(teams, KCR)


HOU = data.frame(name='HOU', 
                 bpERAminus=83, 
                 wRCplus=105,
                 wRCplusL=105,
                 wRCplusR=104,
                 LgRunsPerGame=10651/(162*15),
                 parkFactor=100,
                 defense=11/162,
                 baserunning=7.7/162,
                 starter1ERAminus=94, #McHugh
                 starter1IP=204/32,
                 starter1Hand='R',
                 starter2ERAminus=82,#McCullers 
                 starter2IP=126/22,
                 starter2Hand='R',
                 starter3ERAminus=67, #Keuchel
                 starter3IP=232/33,
                 starter3Hand='L',
                 starter4ERAminus=98, #Fiers
                 starter4IP=175/30,
                 starter4Hand='R'
)


teams =rbind(teams, HOU)
#Keuchel (67, 232/33, 'L')
#McHugh (94, 204/32, 'R')
#Kazmir (93, 183/31, 'L') #but so bad last month probably doesn't get a start
#McCullers(82, 126/22,'R')
#Fiers (98, 175/30,'R')

row.names(teams) <- teams$name
