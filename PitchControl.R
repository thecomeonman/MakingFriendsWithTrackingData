# Porting https://github.com/Friends-of-Tracking-Data-FoTD/LaurieOnTracking/blob/master/Lesson6.py

rm(list = ls())

library(CodaBonito)
library(ggplot2)
library(data.table)
library(scales)
library(zoo)
theme_set(theme_bw(12))

################################################################################
# user parms
################################################################################
{

    # path details to read the data
    cDataRootFolder = '/media/ask/Data/Personal/Projects/Personal/sample-data/data/'
    cGameName = 'Sample_Game_2'

    # pitch dimensions
    nXLimit = 106
    nYLimit = 68

    # upper limit of speed to cap weird speed values
    nUpperLimitSpeed = 10

    # which frame you want to calculate the probabilities for
    # viTrackingFrame = 52865:52900
    # viTrackingFrame = 52936:52900
    # viTrackingFrame = 52865:52965
    # viTrackingFrame = 52865:53165
    viTrackingFrame = seq((52871-5), (52871+50), 5)
    # viTrackingFrame = seq(1672, 1753, 6)

    # to decide which team to consider in attack
    # also used to adjust reaction time in the event that the tracking slice
    # is not at the same instant as the event
    # removed. Is now inferred as the last event before the tracking frame
    # iReferenceEventFrame = 52871

    # resolution of the pitch on the x dimension for calculating probabilities
    # the bigger the number, the smaller the block for which probabilities are
    # calculated, the longer the code runs
    n_grid_cells_x = nXLimit

    vcColourAssignment = c(
        'Home' = 'red',
        'Ball' = 'black',
        'Away' = 'blue'
    )

    cFolderPathToSaveImages = '~/Desktop/PitchControl/'

    # discards the rest of the data after extrcting the frames needed for the 
    # viz
    bKeepOnlySubset = T

}


################################################################################
# parms from Laurie's code
################################################################################
{

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

}



################################################################################
# basic data loading and processing
################################################################################
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



################################################################################
# support functions
################################################################################
{
    
    simple_time_to_intercept = function(
        reaction_time,
        VelocityX,
        VelocityY,
        position_x,
        position_y,
        vmax,
        target_x,
        target_y
    ) {

        position = cbind(position_x, position_y)
        velocity = cbind(VelocityX, VelocityY)
        target = cbind(target_x, target_y)

        # Time to intercept assumes that the player continues moving at current velocity for 'reaction_time' seconds
        # and then runs at full speed to the target position.
        reaction = position + ( velocity * reaction_time )

        time_to_intercept = reaction_time + ( rowSums( ( target - reaction ) ^ 2 ) ^ 0.5 ) / vmax
        # time_to_intercept = reaction_time + ( ( ( ( ( r_final[1] - reaction[, 1] ) ^ 2 ) + ( ( r_final[2] - reaction[, 2] ) ^ 2 ) ) ^ 0.5 ) ) / vmax
        
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

}


################################################################################
# Frame details extraction
################################################################################
{
        
    # iTrackingFrame = lData$dtEventsData[Type == 'PASS', sample(StartFrame, 1)]
    # iTrackingFrame = lData$dtEventsData[821, StartFrame]
    dtTrackingSlice = lData$dtTrackingData[
        Frame %in% viTrackingFrame
    ]

    dtTrackingSlice[,
        c('Period','Time_s') := NULL
    ]
    # last team to make a pass is in control
    # todo
    dtAttackingTeam = merge(
        lData$dtEventsData[
            Type %in% c("PASS", "SHOT", "SET PIECE", "RECOVERY") |
            Subtype %in% c("GOAL KICK", "KICK OFF"),
            list(AttackingTeam = Team[1]),
            list(Frame = StartFrame)
        ],
        data.table(Frame = viTrackingFrame),
        'Frame',
        all = T
    )[, 
        AttackingTeam := na.locf(AttackingTeam, na.rm = F)
    ]

    dtAttackingTeam = dtAttackingTeam[
        Frame %in% viTrackingFrame
    ]

    if ( bKeepOnlySubset ) {

        rm(lData)
        
    }

    if ( F ) {

        p1 = ggplot(
            dtTrackingSlice[Player != 'Ball'][Frame == Frame[1]]
        ) +
            geom_point(aes(x = X, y = Y, color = Tag)) +
            # geom_text(aes(x = X, y = Y, label = Player)) +
            geom_segment(
                data = lData$dtEventsData[StartFrame == dtTrackingSlice[, Frame[1]]],
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

    }

}



################################################################################
# Pitch control probability calculation
################################################################################
{

    vnXArray = seq(0, nXLimit, nXLimit/n_grid_cells_x)
    vnYArray = seq(0, nYLimit, nYLimit / round(nYLimit / ( nXLimit/n_grid_cells_x )))


    dtDetails = data.table(
        expand.grid(
            Frame = viTrackingFrame,
            TargetX = vnXArray,
            TargetY = vnYArray
        )
    )

    dtDetails[, SNO := .I]

    dtDetails = merge(
        dtDetails,
        dtAttackingTeam,
        'Frame'
    )

    dtDetails = merge(
        dtDetails,
        dtTrackingSlice[
            Player == 'Ball'
        ][
            Frame %in% viTrackingFrame
        ][,
            list(
                Frame,
                BallX = X,
                BallY = Y
            )
        ],
        'Frame'
    )

    # ball travel time is distance to target position from current ball position 
    # divided assumed average ball speed 
    # 
    # difference from laurie's code
    # laurie's code used the pass start x,y coordinates but i use the ball's
    # tracked x,y coordinates instead
    dtDetails[, 
        ball_travel_time := sqrt(
            ( ( TargetX - BallX ) ^ 2 ) +
            ( ( TargetY - BallY ) ^ 2 )
        ) / params['average_ball_speed']
    ]

    dtDetails[, c('BallX','BallY') := NULL]

    dtTrackingSliceVectorised = dtTrackingSlice[,
        list(SNO = unlist(dtDetails[, SNO])),
        by = c(colnames(dtTrackingSlice))
    ]

    dtTrackingSliceVectorised = merge(
        dtTrackingSliceVectorised,
        dtDetails,
        c('SNO','Frame')
    )

    # first get arrival time of 'nearest' attacking player (nearest also dependent on current velocity)
    dtTrackingSliceVectorised[
        Tag != 'Ball',
        time_to_intercept := simple_time_to_intercept(
            reaction_time = pmax(
                -Inf,
                params['reaction_time']
                #  - (
                #     Time_s - dtAttackingTeam[,
                #         StartTime_s
                #     ]
                # )
            ),
            VelocityX = VelocityX,
            VelocityY = VelocityY,
            position_x = X,
            position_y = Y,
            vmax = params['max_player_speed'],
            target_x = TargetX,
            target_y = TargetY
        )
    ]

    dtTrackingSliceVectorised[, c('X','Y') := NULL]
    dtTrackingSliceVectorised[, c('TargetX','TargetY') := NULL]
    dtTrackingSliceVectorised[, c('VelocityX','VelocityY') := NULL]

    dtDetails = merge(
        dtDetails,
        setnames(
            dcast(
                dtTrackingSliceVectorised[
                    Tag != 'Ball', 
                    list(
                        tau_min = min(time_to_intercept)
                    ),
                    list(SNO, Tag)
                ], 
                SNO ~ Tag, 
                value.var= 'tau_min'
            ),
            c('Home','Away'),
            c('tau_min_att','tau_min_def')
        ),
        c('SNO')
    )

    dtDetails[
        AttackingTeam == 'Away',
        c('tau_min_att','tau_min_def') := list(tau_min_def, tau_min_att)
    ]

    dtDetails[
        tau_min_att - pmax(ball_travel_time, tau_min_def) >= params['time_to_control_def'],
        AttackProbability := 0
    ]

    dtDetails[
        tau_min_def - pmax(ball_travel_time, tau_min_att) >= params['time_to_control_att'],
        AttackProbability := 1
    ]

    viSNOToEvaluate = dtDetails[(is.na(AttackProbability)), SNO]
    viSNOToEvaluate = sort(viSNOToEvaluate)

    setkey(
        dtTrackingSliceVectorised,
        SNO
    )

    # remove the ones which are already done
    dtTrackingSliceVectorised = merge(
        dtTrackingSliceVectorised,
        dtDetails[
            is.na(AttackProbability), 
            list(SNO, tau_min_att, tau_min_def)
        ],
        'SNO'
    )

    dtTrackingSliceVectorised = dtTrackingSliceVectorised[(
            Tag != AttackingTeam &
            time_to_intercept - tau_min_def < params['time_to_control_def']
        ) | (
            Tag == AttackingTeam &
            time_to_intercept - tau_min_att < params['time_to_control_att']
        )
    ]

    dtTrackingSliceVectorised[, c('tau_min_def','tau_min_att') := NULL]

    dT_array = seq(
        -params['int_dt'],
        params['max_int_time'] - params['int_dt'],
        params['int_dt']
    )

    dtPPCF = data.table(
        AttackProbability = 0,
        DefenseProbability = 0
    )[,
        list(
            SNO = viSNOToEvaluate
        ),
        list(
            DefenseProbability,
            AttackProbability
        )
    ]

    vbSNOToEvaluationFlag = rep(T, length(viSNOToEvaluate))
    dtTrackingSliceVectorised[, PlayerPPCF := 0]
    
    i = 2

    dtProbabilities = data.table()

    repeat {

        # calculate ball control probablity for 'player' in time interval T+dt
        dtTrackingSliceVectorised = merge(
            dtTrackingSliceVectorised,
            dtPPCF,
            c('SNO'),
            all.x = T
        )
        
        dtTrackingSliceVectorised[
            !is.na(AttackProbability), 
            PlayerPPCF := 
                PlayerPPCF + 
                pmax(
                    ( 
                        1 - 
                        AttackProbability - 
                        DefenseProbability
                    ) *
                    probability_intercept_ball(
                        params['tti_sigma'],
                        time_to_intercept,
                        dT_array[i] + ball_travel_time
                    ) *
                    params[
                        paste0(
                            'lambda_',
                            ifelse(
                                Tag == AttackingTeam,
                                'att',
                                'def'
                            )
                        )
                    ] * 
                    params['int_dt'],
                    0
                )
        ]

        dtPPCF = dtTrackingSliceVectorised[
            !is.na(AttackProbability),
            list( 
                DefenseProbability = sum(
                    PlayerPPCF[Tag != AttackingTeam]
                ), 
                AttackProbability = sum(
                    PlayerPPCF[Tag == AttackingTeam]
                )
            ),
            SNO
        ]

        dtProbabilities = rbind(
            dtProbabilities,
            dtPPCF[
                AttackProbability + DefenseProbability > 1 - params['model_converge_tol'],
                list(
                    SNO,
                    AttackProbability,
                    DefenseProbability
                )
            ],
            fill = T
        )

        if ( dtProbabilities[, any(AttackProbability + DefenseProbability > 1 + params['model_converge_tol'])] ) {
            stop('Probabilities > 1. Look at dtProbabilities to debug.')
        }


        dtPPCF = dtPPCF[
            !SNO %in% dtProbabilities[, SNO]
        ]

        i = i + 1

        if ( i > length(dT_array) ) {
            break
        }

        if ( nrow(dtPPCF) == 0 ) {
            break
        }

        dtTrackingSliceVectorised = dtTrackingSliceVectorised[,
            c('AttackProbability','DefenseProbability') := NULL
        ]

    }


    if ( i > length(dT_array) ) {

        warning(
            paste0(
                "Integration failed to converge for some cases",
                ptot
            )
        )

        print(
            paste0(
                'SNOS:',
                dtPPCF[, SNO]
            )
        )

        # stop()

    }

    dtDetails = rbind(
        dtDetails[!is.na(AttackProbability)],
        merge(
            dtDetails[
                is.na(AttackProbability)
            ][, 
                !'AttackProbability'
            ],
            dtProbabilities,
            'SNO',
            all = T
        ),
        fill = T
    )

    dtDetails[
        is.na(DefenseProbability),
        DefenseProbability := 1 - AttackProbability
    ]

}


################################################################################
# Plot of pitch control
################################################################################
{
        
    cFolderPathToSaveImagesSubdir = paste0(
        cFolderPathToSaveImages,
        min(viTrackingFrame), '_',
        max(viTrackingFrame), '/'
    )

    dir.create(
        cFolderPathToSaveImagesSubdir,
        recursive = T,
        showWarnings = F
    )

    for ( iTrackingFrame in viTrackingFrame ) {
            
        p1 = ggplot() +
            geom_tile(
                data = dtDetails[
                    Frame == iTrackingFrame
                ],
                aes(
                    x = TargetX,
                    y = TargetY,
                    fill =  AttackProbability
                )
            ) +
            geom_point(
                data = dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player != 'Ball'
                ],
                aes(x = X, y = Y),
                color = 'white',
                size = 7
            ) +
            geom_point(
                data = dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player != 'Ball'
                ],
                aes(x = X, y = Y, color = Tag),
                size = 6
            ) +
            geom_point(
                data = dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player == 'Ball'
                ],
                aes(x = X, y = Y, color = Tag),
                size = 4
            ) +
            geom_segment(
                data = dtTrackingSlice[
                    Frame == iTrackingFrame
                ][
                    Player != 'Ball'
                ],
                aes(
                    x = X, y = Y,
                    xend = X + VelocityX,
                    yend = Y + VelocityY,
                    color = Tag
                )
            ) +
            # geom_text(aes(x = X, y = Y, label = Player)) +
            # geom_segment(
            #    data = lData$dtEventsData[StartFrame == iTrackingFrame],
            #    aes(
            #       x = EventStartX,
            #       y = EventStartY,
            #       xend = EventEndX,
            #       yend = EventEndY
            #    )
            # ) +
            scale_fill_gradient2(
                low = vcColourAssignment[
                    dtAttackingTeam[Frame == iTrackingFrame, setdiff(c('Home','Away'), AttackingTeam)]
                ], 
                mid = 'white', 
                high = vcColourAssignment[
                    dtAttackingTeam[Frame == iTrackingFrame, AttackingTeam]
                ],
                midpoint = 0.5,
                guide = FALSE
            ) +
            scale_color_manual(
                values = vcColourAssignment,
                guide = FALSE
            )

        p1 = fAddPitchLines(
            p1,
            nXLimit = nXLimit,
            nYLimit = nYLimit,
            cLineColour = 'black',
            cPitchColour = NA
        )

        p1 = p1 + 
            theme_pitch()

        # print(p1)

        ggsave(
            p1,
            file = paste0(
                cFolderPathToSaveImagesSubdir,
                iTrackingFrame,
                '.png'
            ),
            width = 20,
            height = 14,
            units = 'cm'
        )

    }

}