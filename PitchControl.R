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
    nXLimit = 120
    nYLimit = 80

    # upper limit of speed to cap weird speed values
    nUpperLimitSpeed = 5 * 120 / 105

    # which frame you want to calculate the probabilities for
    viTrackingFrame = seq((52871-5), (52936+5), 5)
    # viTrackingFrame = 52871
    # viTrackingFrame = seq(1672, 1753, 6) # possession changes in this

    # to decide which team to consider in attack
    # also used to adjust reaction time in the event that the tracking slice
    # is not at the same instant as the event
    # removed. Is now inferred as the last event before the tracking frame
    # iReferenceEventFrame = 52871

    # resolution of the pitch on the x dimension for calculating probabilities
    # the bigger the number, the smaller the block for which probabilities are
    # calculated, the longer the code runs
    iGridCellsX = nXLimit

    vcColourAssignment = c(
        'Home' = 'red',
        'Ball' = 'black',
        'Away' = 'blue'
    )

    cFolderPathToSaveImages = '~/Desktop/PitchControl/'

}


################################################################################
# parms from Laurie's code
################################################################################
{

    params = c()
    params['time_to_control_veto'] = 3
    # model parameters
    params['max_player_accel'] = 7. # maximum player acceleration m/s/s, not used in this implementation
    params['max_player_speed'] = nUpperLimitSpeed # maximum player speed m/s
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
    # params['time_to_control_att'] = params['time_to_control_veto']*np.log(10) * (np.sqrt(3)*params['tti_sigma']/np.pi + 1/params['lambda_att'])
    # params['time_to_control_def'] = params['time_to_control_veto']*np.log(10) * (np.sqrt(3)*params['tti_sigma']/np.pi + 1/params['lambda_def'])
    params['time_to_control_att'] = params['time_to_control_veto']*log(10) * (sqrt(3)*params['tti_sigma']/pi + 1/params['lambda_att'])
    params['time_to_control_def'] = params['time_to_control_veto']*log(10) * (sqrt(3)*params['tti_sigma']/pi + 1/params['lambda_def'])

}



################################################################################
# data loading
################################################################################
{

    # loading the data in
    lData = fParseTrackingDataBothTeams(
        cRootPath = cDataRootFolder,
        cGameName = cGameName,
        nXLimit = nXLimit,
        nYLimit = nYLimit,
        xMaxBB = 1,
        yMaxBB = 1,
        nUpperLimitSpeed = nUpperLimitSpeed
    )

}





################################################################################
# Plot of pitch control
################################################################################
{

    lPitchControl = fGetPitchControlProbabilities (
        lData,
        viTrackingFrame,
        params = params,
        nYLimit = nYLimit,
        nXLimit = nXLimit,
        iGridCellsX = iGridCellsX
    )
        
    cFolderPathToSaveImagesSubdir = paste0(
        cFolderPathToSaveImages,
        min(viTrackingFrame), '_',
        max(viTrackingFrame), '/'
    )

    pPlotPitchControl = fPlotPitchControl(
        lPitchControl,
        cFolderPathToSaveImagesSubdir
    )

    print(pPlotPitchControl)

}