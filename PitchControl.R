# pretty much a copy of https://github.com/Friends-of-Tracking-Data-FoTD/LaurieOnTracking/blob/master/Lesson6.py

rm(list = ls())

library(CodaBonito)
library(ggplot2)
library(data.table)
library(scales)
theme_set(theme_bw(12))

cDataRootFolder = '/media/ask/Data/Personal/Projects/Personal/sample-data/data/'
cGameName = 'Sample_Game_2'
nXLimit = 106
nYLimit = 68
nMovingCutoff = 2
nRunningCutoff = 2
nUpperLimitSpeed = 15
iFrame = 52871


# basic data loading and processing
{

   # loading the data in
   lData = fParseTrackingDataBothTeams(
      cRootPath = cDataRootFolder,
      cGameName = cGameName,
      nXLimit = nXLimit,
      nYLimit = nYLimit,
      xMaxBB = 1,
      yMaxBB = 1
   )


   lData$dtTrackingData[,
      Velocity := c(
         0,
         (
            (
               ( diff(X) ^ 2 ) +
               ( diff(Y) ^ 2 )
            ) ^ 0.5
         ) / diff(Time_s)
      ),
      list(
         Player
      )
   ]


   lData$dtTrackingData[,
      VelocityX := c(
         0,
         diff(X) / diff(Time_s)
      ),
      list(
         Player
      )
   ]

   lData$dtTrackingData[,
      VelocityY := c(
         0,
         diff(Y) / diff(Time_s)
      ),
      list(
         Player
      )
   ]

   lData$dtTrackingData[
      Velocity > nUpperLimitSpeed,
      Velocity := params['max_player_speed']
   ]

}











# parms

time_to_control_veto = 3

params = c()
# model parameters
params['max_player_accel'] = 7. # maximum player acceleration m/s/s, not used in this implementation
params['max_player_speed'] = 5. # maximum player speed m/s
params['reaction_time'] = 0.7 # seconds, time taken for player to react and change trajectory. Roughly determined as vmax/amax
params['tti_sigma'] = 0.45 # Standard deviation of sigmoid function in Spearman 2018 ('s') that determines uncertainty in player arrival time
params['kappa_def'] =  1. # kappa parameter in Spearman 2018 (=1.72 in the paper) that gives the advantage defending players to control ball, I have set to 1 so that home & away players have same ball control probability
params['lambda_att'] = 4.3 # ball control parameter for attacking team
params['lambda_def'] = 4.3 * params['kappa_def'] # ball control parameter for defending team
params['average_ball_speed'] = 15. # average ball travel speed in m/s
# numerical parameters for model evaluation
params['int_dt'] = 0.04 # integration timestep (dt)
params['max_int_time'] = 10 # upper limit on integral time
params['model_converge_tol'] = 0.01 # assume convergence when PPCF>0.99 at a given location.
# The following are 'short-cut' parameters. We do not need to calculated PPCF explicitly when a player has a sufficient head start.
# A sufficient head start is when the a player arrives at the target location at least 'time_to_control' seconds before the next player
# params['time_to_control_att'] = time_to_control_veto*np.log(10) * (np.sqrt(3)*params['tti_sigma']/np.pi + 1/params['lambda_att'])
# params['time_to_control_def'] = time_to_control_veto*np.log(10) * (np.sqrt(3)*params['tti_sigma']/np.pi + 1/params['lambda_def'])
params['time_to_control_att'] = time_to_control_veto*log(10) * (sqrt(3)*params['tti_sigma']/pi + 1/params['lambda_att'])
params['time_to_control_def'] = time_to_control_veto*log(10) * (sqrt(3)*params['tti_sigma']/pi + 1/params['lambda_def'])




simple_time_to_intercept = function(
   reaction_time,
   VelocityX,
   VelocityY,
   position_x,
   position_y,
   vmax,
   r_final
) {

   position = cbind(position_x, position_y)
   velocity = cbind(VelocityX, VelocityY)

   # Time to intercept assumes that the player continues moving at current velocity for 'reaction_time' seconds
   # and then runs at full speed to the target position.
   r_reaction = position + ( velocity * reaction_time )
   time_to_intercept = reaction_time + ( rowSums( ( r_final - r_reaction ) ^ 2 ) ^ 0.5 ) / vmax
   return ( time_to_intercept )

}



probability_intercept_ball = function(
   tti_sigma,
   time_to_intercept,
   Time
) {

   # probability of a player arriving at target location at time 'T' given their expected time_to_intercept (time of arrival), as described in Spearman 2018
   f = 1/(1. + exp( -pi/sqrt(3.0)/tti_sigma * (Time - time_to_intercept) ) )
   return (f)

}








# iFrame = lData$dtEventsData[Type == 'PASS', sample(StartFrame, 1)]
# iFrame = lData$dtEventsData[821, StartFrame]
dtTrackingSlice = lData$dtTrackingData[
   Frame %in% iFrame
]
cAttackingTeam = lData$dtEventsData[StartFrame == iFrame, Team[1]]
cDefendingTeam = setdiff(
   lData$dtEventsData[, unique(Team)],
   cAttackingTeam
)

p1 = ggplot(
   dtTrackingSlice[Player != 'Ball']
) +
   geom_point(aes(x = X, y = Y, color = Tag)) +
   # geom_text(aes(x = X, y = Y, label = Player)) +
   geom_segment(
      data = lData$dtEventsData[StartFrame == iFrame],
      aes(
         x = EventStartX,
         y = EventStartY,
         xend = EventEndX,
         yend = EventEndY
      )
   ) +
   scale_color_manual(
      values = c('Home' = 'red','Ball' = 'black','Away' = 'blue'),
      guide = FALSE
   )

p1 = fAddPitchLines(
   p1,
   nXLimit = nXLimit,
   nYLimit = nYLimit,
   cLineColour = 'black',
   cPitchColour = NA
)

print(p1)

n_grid_cells_x = 49

vnXArray = seq(0, nXLimit, nXLimit/n_grid_cells_x)
vnYArray = seq(0, nYLimit, nYLimit / round(nYLimit / ( nXLimit/n_grid_cells_x )))

cTempDir = tempdir()
dir.create(cTempDir)
dtPitchControlProbabilities = rbindlist(
   lapply(
      0:(length(vnXArray)-1),
      # vnXArray[16],
      function( j ) {

         cFileName = paste0(
            cTempDir,
            '/PitchControlProbability',
            j,
            '.Rdata'
         )

         if ( file.exists(cFileName) ) {
            load(cFileName)
         } else {

            dtProbability = rbindlist(
               lapply(
                  0:(length(vnYArray)-1),
                  # vnYArray[16],
                  function( i ) {

                     # i=0;j=4

                     target_position = c()
                     target_position['x'] = vnXArray[j + 1]
                     target_position['y'] = vnYArray[i + 1]

                     # print(target_position)
                     print(
                        paste0(
                           'i=',i,';j=',j
                        )
                     )

                     # ball travel time is distance to target position from current ball position divided assumed average ball speed
                     ball_travel_time = sqrt(
                        ( ( target_position['x'] - lData$dtEventsData[StartFrame == iFrame, EventStartX] ) ^ 2 ) +
                        ( ( target_position['y'] - lData$dtEventsData[StartFrame == iFrame, EventStartY] ) ^ 2 )
                        # ( ( target_position['x'] - dtTrackingSlice[Player == 'Ball', X] ) ^ 2 ) +
                        # ( ( target_position['y'] - dtTrackingSlice[Player == 'Ball', Y]) ^ 2 )
                     ) / params['average_ball_speed']
                     ball_travel_time = unname(ball_travel_time)

                     # first get arrival time of 'nearest' attacking player (nearest also dependent on current velocity)
                     for ( i in seq(nrow(dtTrackingSlice)) ) {

                        dtTrackingSlice[
                           i,
                           time_to_intercept := simple_time_to_intercept(
                              reaction_time = params['reaction_time'],
                              VelocityX = VelocityX,
                              VelocityY = VelocityY,
                              position_x = X,
                              position_y = Y,
                              vmax = params['max_player_speed'],
                              r_final = target_position
                           )
                        ]

                     }

                     tau_min_def = dtTrackingSlice[
                        Tag == cDefendingTeam, min(time_to_intercept)
                     ]

                     tau_min_att = dtTrackingSlice[
                        Tag == cAttackingTeam, min(time_to_intercept)
                     ]

                     if ( tau_min_att - max(ball_travel_time, tau_min_def) >= params['time_to_control_def'] ) {
                        # if defending team can arrive significantly before attacking team, no need to solve pitch control model


                        dtProbability = data.table(
                           TargetX = target_position['x'],
                           TargetY = target_position['y'],
                           AttackProbability = 0,
                           DefenseProbability = 1
                        )

                     } else if ( tau_min_def - max(ball_travel_time,tau_min_att) >= params['time_to_control_att'] ) {
                        # if attacking team can arrive significantly before defending team, no need to solve pitch control model


                        dtProbability = data.table(
                           TargetX = target_position['x'],
                           TargetY = target_position['y'],
                           AttackProbability = 1,
                           DefenseProbability = 0
                        )

                     } else {

                        attacking_players = dtTrackingSlice[
                           Tag == cAttackingTeam &
                           time_to_intercept - tau_min_att < params['time_to_control_att'],
                           Player
                        ]

                        defending_players = dtTrackingSlice[
                           Tag == cDefendingTeam &
                           time_to_intercept - tau_min_def < params['time_to_control_def'],
                           Player
                        ]

                        dT_array = seq(
                            ball_travel_time - params['int_dt'],
                            ball_travel_time + params['max_int_time'] - params['int_dt'],
                            params['int_dt']
                        )

                        PPCFatt = rep(0, length(dT_array))
                        PPCFdef = rep(0, length(dT_array))

                        ptot = 0.0
                        i = 2

                        dtTrackingSlice[, PPCF := 0]

                        repeat {

                           if ( !(1 - ptot > params['model_converge_tol'] & i <= length(dT_array) ) ) {
                              break
                           }

                           Time = dT_array[i]

                           for ( player in attacking_players ) {

                              # calculate ball control probablity for 'player' in time interval T+dt
                              dPPCFdT = ( 1 - PPCFatt[i-1] - PPCFdef[i-1] ) *
                                 probability_intercept_ball(
                                    params['tti_sigma'],
                                    dtTrackingSlice[Player == player, time_to_intercept],
                                    Time
                                 ) *
                                 params['lambda_att']
                              # print(dPPCFdT)


                              # make sure it's greater than zero
                              dPPCFdT = pmax(dPPCFdT, 0)
                              dtTrackingSlice[
                                 player == Player,
                                 PPCF := PPCF + ( dPPCFdT * params['int_dt'] )
                              ] # total contribution from individual player

                              PPCFatt[i] = PPCFatt[i] +
                                 dtTrackingSlice[
                                    player == Player,
                                    PPCF
                                 ] # add to sum over players in the attacking team (remembering array element is zero at the start of each integration iteration)

                           }

                           for ( player in defending_players ) {

                              # calculate ball control probablity for 'player' in time interval T+dt
                              dPPCFdT = ( 1 - PPCFatt[i-1] - PPCFdef[i-1] ) *
                                 probability_intercept_ball(
                                    params['tti_sigma'],
                                    dtTrackingSlice[Player == player, time_to_intercept],
                                    Time
                                 ) *
                                 params['lambda_def']

                              # make sure it's greater than zero
                              dPPCFdT = pmax(dPPCFdT, 0)
                              dtTrackingSlice[
                                 player == Player,
                                 PPCF := PPCF + ( dPPCFdT * params['int_dt'] )
                              ] # total contribution from individual player

                              PPCFdef[i] = PPCFdef[i] +
                                 dtTrackingSlice[
                                    player == Player,
                                    PPCF
                                 ] # add to sum over players in the attacking team (remembering array element is zero at the start of each integration iteration)

                           }

                           ptot = PPCFdef[i] + PPCFatt[i] # total pitch control probability
                           i = i + 1

                        }


                        if ( i > length(dT_array) ) {

                           print(PPCFatt)
                           print(PPCFdef)

                           warning(
                              paste0(
                                 "Integration failed to converge:",
                                 ptot
                              )
                           )

                           stop()

                        }

                        if ( ptot > 1 + params['model_converge_tol'] ) {
                           warning('ptot is weird')
                           stop()
                        }

                        dtProbability = data.table(
                           TargetX = target_position['x'],
                           TargetY = target_position['y'],
                           AttackProbability = PPCFatt[i-1],
                           DefenseProbability = PPCFdef[i-1]
                        )

                        print(dtProbability)
                        print(ptot)
                        print('-------------------')

                        dtTrackingSlice[, PPCF := NULL]
                        rm(player)
                        rm(ptot)
                        rm(PPCFatt)
                        rm(PPCFdef)
                        rm(attacking_players)
                        rm(ball_travel_time)
                        rm(dPPCFdT)
                        rm(defending_players)
                        rm(dT_array)
                        rm(i)
                        rm(j)
                        rm(time_to_intercept)
                        rm(target_position)
                        rm(TargetY)
                        rm(TargetY)
                        rm(tau_min_def)
                        rm(tau_min_att)
                        rm(ball_travel_time)
                     }

                     dtProbability

                  }
               )
            )

         }

         save(
            dtProbability,
            file = cFileName
         )

         dtProbability

      }
   )
)

p1 = ggplot() +
   geom_tile(
      data = dtPitchControlProbabilities,
      aes(
         x = TargetX,
         y = TargetY,
         fill =  AttackProbability
      )
   ) +
   geom_point(
      data = dtTrackingSlice[Player != 'Ball'],
      aes(x = X, y = Y, color = Tag),
      size = 3
   ) +
   # geom_text(aes(x = X, y = Y, label = Player)) +
   # geom_segment(
   #    data = lData$dtEventsData[StartFrame == iFrame],
   #    aes(
   #       x = EventStartX,
   #       y = EventStartY,
   #       xend = EventEndX,
   #       yend = EventEndY
   #    )
   # ) +
   scale_fill_gradient2(
       low = 'red', mid = 'white', high = 'blue', midpoint = 0.5
   ) +
   scale_color_manual(
      values = c('Home' = 'red','Ball' = 'black','Away' = 'blue'),
      guide = FALSE
   )

p1 = fAddPitchLines(
   p1,
   nXLimit = nXLimit,
   nYLimit = nYLimit,
   cLineColour = 'black',
   cPitchColour = NA
)

print(p1)
