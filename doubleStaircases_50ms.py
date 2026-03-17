

# imports we will probably need...
from psychopy import visual, event, monitors

import numpy as np
import random, os, sys, time, json
import pandas as pd

from serial import Serial


def runExperiment(ID=None):

    # every function, except this one, will get a cfg dictionary as input
    # it will change it, and then also return it as output

    # here we set it up:

    cfg = {}

    # there will be 2 parts of this cfg dictionary:
    # 1. a part that is a more or less "plain text" dictionary,
    #    with objects that can be stored in a JSON
    #    this will be called "state"
    # 2. a part that has binary objects that can not be stored in a JSON
    #    most notably the psychopy window, and stimulus objects
    #    this will be called "bin"

    # after every trial, we store the state, and we also do this in case of exceptions
    # eventually, we'll write a function that can restart the experiment where it 
    # crashed, if that should ever happen, and for this we use the last 'state' that was saved

    cfg['state'] = {}  # want to store
    cfg['bin']   = {}  # can not store

    cfg['ext']   = {}  # do not want to store

    cfg['state']['ID'] = ID

    # everything else we do will live in a try / except / else / finally block:

    try:

        cfg = prepare(cfg)

        # cfg = runTasks(cfg)

        cfg = runFamiliarization(cfg)

        cfg = runStaircases(cfg)

    except Exception as e:

        # something went wrong, save the state part of the cfg:

        print(e)

        cfg = saveState(cfg)

        # close the psychopy environment (if still possible)

    else:

        # experiment finished, do final steps:

        # we combine all trial data into a full description
        # cfg = combineData(cfg)

        # and maybe generate a summary with one line per trial?
        # cfg = createSummary(cfg)
        # all psychophysics is in one file already, no need to summarize
        pass

    finally:

        # still also need to close the environment:
        cfg = closeEnvironment(cfg)



def prepare(cfg):

    cfg = setupParticipant(cfg)

    cfg = setupEnvironment(cfg)

    # cfg = setupTasks(cfg)

    cfg = setupStaircases(cfg) # includes response dictionary?

    print("preparation finished")

    cfg = saveState(cfg)

    return(cfg)

def setupParticipant(cfg):

    if cfg['state']['ID'] == None:
        print("participant ID not defined")
        raise Exception("participant ID not defined")

    dataFolder = "data/%s/"%(cfg['state']['ID'])

    os.makedirs("data", exist_ok = True)
    if os.path.isdir(dataFolder):
        # participant folder exists
        cfg["state"]["crashRecovery"] = False
        if os.path.isfile("%sstate.json"%(dataFolder)):
            cfg["state"]["crashRecovery"] = True
    else:
        os.makedirs(dataFolder, exist_ok = True)
    cfg["state"]["dataFolder"] = dataFolder

    random.seed(cfg['state']['ID'])  

    # check if participant directory already exists

    # if not, create

    # if it does exist, check if there is a state.json file

    # if there is, check if it finished the experiment, or not
    # (this means we should probably store that the state)

    # if it's not finished, set a toggle for setupTasks, to restore the tasks from the json

    # the environment will still need to be created, because it's not store in the json


    # the other thing that depends on the participant ID is the state of the random-number generator
    # we know the initial state of the random number generator, because we set it with a seed depending on the participant ID

    # what we would not know is the state of the random number generator after it has done stuff, so..
    # if there is an `rngstate` in the state.json, we can `random.setstate(cfg['state']['rngstate'])`
    # and have that right where we left it, so that the experiment will continue exactly as if it hadn't crashed

    # if it's not there, we set it as best as we can

    return(cfg)


def setupEnvironment(cfg):


    # here we do a few things, that each deserve their own function as well
    # all of this will end up in the cfg['bin']

    cfg = setupPsychopyWindow(cfg)

    cfg = setupTabletTracker(cfg)

    cfg = setupTVFU(cfg)

    cfg = setupStimuli(cfg)


    return(cfg)

def setupPsychopyWindow(cfg):

    # this should be a psychopy window object that has the units as centimeters
    myMonitor = monitors.Monitor(name='temp',
                                 distance=100,
                                 width=52.7)
    myMonitor.setSizePix([1920,1080])

    cfg['bin']['win'] = visual.Window( size=[1920,1080], 
                                fullscr=True, 
                                units='cm', 
                                waitBlanking=False, 
                                #viewScale=[1,-1], 
                                color=[-1,-1,-1], 
                                screen=0, 
                                monitor=myMonitor)

    #   "size_px"         : [1920, 1080], 
    #   "size_cm"         : [52.7, 29.6],
    #   "viewscale"       : [1,-1],

    # for testing on non-mirrored setup:
    #cfg['win'] = visual.Window(fullscr=True, units='pix', waitBlanking=True, viewScale=[1,1], color=[-1,-1,-1])

    # set up the workspace as a function of the size of the window:
    # winSize = cfg['win'].size

    # we want 8 cm reaches
    # if we apply the viewscale correctly, that should be possible
    # leaving 3.375/2 cm free on top and bottom
    # the monitor on the tablet setup is 1920 pixels wide,
    # and that should span 31 cm on the tablet surface

    # cfg['xPPC'] = 1920/52.7
    # cfg['yPPC'] = 1080/29.6

    return(cfg)
    
 # set up 'mouse' object to track reaches:
class myMouse:

    # TABLET:
    # "size_px"    : [1920, 1080],
    # "size_cm"    : [31.1, 21.6],
    # "mapping"    : 'relative',     <-   this is not true right now

    # MONITOR:
    #   "size_px"         : [1920, 1080], 
    #   "size_cm"         : [52.7, 29.6],
    #   "viewscale"       : [1,-1],

    def __init__(self,cfg):
        # we use a psychopy mouse object
        self.psyMouse = event.Mouse(visible = False, newPos = None, win = cfg['bin']['win'])
        self.xfactor = 52.7/31.1
        self.yfactor = 29.6/21.6

    def getPos(self):
        # but in addition to the position, we also return the time the position was asked for
        [X,Y] = self.psyMouse.getPos()
        st = time.time()
        X = X / self.xfactor # scale to centimeters ?
        Y = Y / self.yfactor # scale to centimeters ?

        return [X,Y,st]

def setupTabletTracker(cfg):

    # create a custom getpos function
    # using the mouse.getPos thing internally
    # but transform to centimeters on the tablet
    # and add a timestamp, so we can store the stylus position with the time it was polled
    # for the most accurate estimates of speed, acceleration etc possible
    # make them system timestamps in UNIX time (seconds since jan 1st 1980)
    # so that we also know exactly when the participant did the task

    cfg['bin']['tracker'] = myMouse(cfg)

    return(cfg)

def setupTVFU(cfg):

    # this is assumed to be the Arduino-based Tactile Vibration Feedback Unit
    # running on our modified / extended version of the Arduino sketch
    # that allows setting intensity and duration as any allowed value

    cfg['bin']['TVFU'] = Serial( port     = '/dev/ttyACM0',
                                 baudrate = 115200          )

    return(cfg)


def setupStimuli(cfg):

    # this creates the stimulu we'll use, for now there are at least 4:

    # 1. a cursor (circle)
    cfg['bin']['cursor'] = visual.Circle(  win=cfg['bin']['win'], 
                                    radius=1, 
                                    lineWidth=0, 
                                    lineColorSpace='rgb', 
                                    lineColor=None, 
                                    fillColorSpace='rgb', 
                                    fillColor='#990000'     )
    # 2. a start postition (circle)
    cfg['bin']['start'] = visual.Circle(  win=cfg['bin']['win'], 
                                    radius=0.5, 
                                    lineWidth=2, 
                                    lineColorSpace='rgb', 
                                    lineColor='#999999', 
                                    fillColorSpace='rgb', 
                                    fillColor=None          )
    # 3. a target position (circle)
    cfg['bin']['target'] = visual.Circle(  win=cfg['bin']['win'], 
                                    radius=1, 
                                    lineWidth=2, 
                                    lineColorSpace='rgb', 
                                    lineColor='#999999', 
                                    fillColorSpace='rgb', 
                                    fillColor=None          )
    # 4. an empty text box for on-screen instructions
    cfg['bin']['instruction'] = visual.TextStim(win=cfg['bin']['win'], text='hello world', pos=[0,0], colorSpace='rgb', color='#999999')


    # these are all psychopy stim objects, associated with the window we created


    return(cfg)



def foldout(values, names):
    # http://code.activestate.com/recipes/496807-list-of-all-combination-from-multiple-lists/



    r=[[]]
    for x in values:
        r = [ i + [y] for y in x for i in r ]

    # return(pd.DataFrame(r))

    df = pd.DataFrame(r)
    df.columns = names
    return(df.to_dict())

# def setupTasks(cfg):

#     leftTargets = [(-10, 5), (-10, -5)]
#     rightTargets = [(10, 5), (10, -5)]

#     cfg['state']['leftTargets'] = leftTargets
#     cfg['state']['rightTargets'] = rightTargets

#     # master list of all conditions, positon, size, tactile stim
#     leftConditions = foldout(values = [[1, 0.5], 
#                                     [False, 1/3, 2/3], 
#                                     leftTargets],
#                          names = ["targetSize", "tactileStim", "targetPos"])
#     rightConditions = foldout(values = [[1, 0.5], 
#                                     [False, 1/3, 2/3], 
#                                     rightTargets],
#                          names = ["targetSize", "tactileStim", "targetPos"])
#     conditions = pd.concat([pd.DataFrame(leftConditions), pd.DataFrame(rightConditions)], ignore_index = True).to_dict()
#     cfg["state"]["conditions"] = conditions

#     # create a randomized list of trials unique to each participant ID

#     Nconditions = len(conditions[list(conditions.keys())[0]])
#     CondIdxOne = list(range(int(Nconditions/2)))
#     CondIdxTwo = list(range(int(Nconditions/2), Nconditions))
#     if random.sample([True, False],1):
#         CondIdxOne, CondIdxTwo = CondIdxTwo, CondIdxOne

#     # two pseudo-randomized blocks

#     trialOrder = []
#     for blockNo in range(2):
#         random.shuffle(CondIdxOne)
#         random.shuffle(CondIdxTwo)
#         for Idx in range(len(CondIdxOne)):
#             trialOrder.append(CondIdxOne[Idx])
#             trialOrder.append(CondIdxTwo[Idx])

#     cfg["state"]["trialOrder"] = trialOrder

#     return(cfg)

def setupStaircases(cfg):
    
    # varying strengths:
    steps = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115, 117, 119, 121, 123, 125, 127]
    
    minTrials = 40
    minReversals = 12

    staircases = []

    # for duration in  [17, 25, 33, 41, 50]:
    # for duration in  [25, 41, 50]:
    duration = 50

    info = {'motor'     : 2,
            'duration'  : duration,
            'stepvalue' : 'strength'}

    staircases.append( DescreasingStepStaircase(
        steps        = list(range(1,128,3)),
        idx          = 0,
        minTrials    = minTrials,
        minReversals = minReversals,
        step         = 8,
        decrease     = 1,
        info         = info
    ) )

    staircases.append( DescreasingStepStaircase(
        steps        = list(range(1,128,3)),
        idx          = len(range(1,128,3))-1,
        minTrials    = minTrials,
        minReversals = minReversals,
        step         = 8,
        decrease     = 1,
        info         = info
    ) )

    staircases.append( DescreasingStepStaircase(
        steps        = list(range(1,128,3)),
        idx          = 0,
        minTrials    = minTrials,
        minReversals = minReversals,
        step         = 8,
        decrease     = 1,
        info         = info
    ) )

    staircases.append( DescreasingStepStaircase(
        steps        = list(range(1,128,3)),
        idx          = len(range(1,128,3))-1,
        minTrials    = minTrials,
        minReversals = minReversals,
        step         = 8,
        decrease     = 1,
        info         = info
    ) )

    # staircases.append( UDStaircase(
    #         steps        = steps,
    #         idx          = 0,
    #         minTrials    = minTrials,
    #         minReversals = minReversals,
    #         up           = 5,
    #         down         = 1,
    #         info         = info
    #     ) )

    # staircases.append( UDStaircase(
    #         steps        = steps,
    #         idx          = len(steps) - 1,
    #         minTrials    = minTrials,
    #         minReversals = minReversals,
    #         up           = 1,
    #         down         = 5,
    #         info         = info
    #     ) )




    cfg['bin']['staircases'] = staircases

    cfg['state']['responses'] = {'trial'      : [],
                                 'staircase'  : [],
                                 'strength'   : [],
                                 'duration'   : [],
                                 'timejitter' : [],
                                 'response'   : [],
                                 }

    return(cfg)

# def runTasks(cfg):
#     print("runTasks")
#     print(len(cfg["state"]["trialOrder"]))
#     trialOrder = cfg['state']['trialOrder']
#     nTrials = len(trialOrder)
#     for trialNumber in range(nTrials):
#         print(trialNumber)
#         cfg['state']['trialNumber'] = trialNumber

#         # show instructions if applicable

#         cfg = runTrial(cfg)
#        #  cfg = saveState(cfg)

#     return(cfg)

def runFamiliarization(cfg):


        # cfg['bin']['instruction'] = visual.TextStim(win=cfg['bin']['win'], text='hello world', pos=[0,0], colorSpace='rgb', color='#999999')
    cfg['bin']['instruction'].text = 'On each trial, you will see a red dot, and you may also feel a vibration. When the dot changes from red to blue, press the RIGHT ARROW key if you felt a vibration, and the LEFT ARROW key if you did not. Press SPACE to start the familiarization block.'
    cfg['bin']['instruction'].draw()
    cfg['bin']['win'].flip()

    # wait for press of the space bar:
    k = ['wait']
    while k[0] not in ['space', 'q']:
        k = event.waitKeys()
        # cfg['bin']['instruction'].draw()
        # cfg['bin']['win'].flip()

    if k[0] in ['q']:
        cfg['ext']['quit'] = True


    

    strengths = [0,0,0,0,127,127,127,127]
    random.shuffle(strengths)

    motor = 2
    duration = 200

    for strength in strengths:

        prepcommand = 'M%d.S%d.D%d.'%(motor, strength, duration)

        cfg['bin']['TVFU'].write(bytes(prepcommand, 'utf-8'))

        # blank screen
        blank = 0.5
        # stimulus cue
        # wait interval
        # stimulus jitter
        jitter = random.uniform(0, 1)
        # stimulus presentation
        stimulusOnset = blank + jitter
        # response cue
        cueTime = blank + 1.5
        # get response
        # store data and end trial
        startTime = time.time()

        presentStimulus = True
        inStimulusInterval = True
        cfg['bin']['cursor'].fillColor = '#ff0000'


        while inStimulusInterval:
            now = time.time()
            if now > (startTime + stimulusOnset):
                # do tactile stimulation
                if (presentStimulus):
                    presentStimulus = False
                    cfg['bin']['TVFU'].write(bytes('G2.', 'utf-8'))

                pass
            if now > (startTime + cueTime):
                cfg['bin']['cursor'].fillColor = '#0000ff'
                inStimulusInterval = False
            if now > (startTime + blank):
                cfg['bin']['cursor'].draw()
            cfg['bin']['win'].flip()
    
        # wait for response
        k = ['wait']

        # left  = no stimulus detected  -> go up the staircase
        # right = detected stimulus     -> go down the staircase

        correct = True

        while k[0] not in ['left', 'right', 'q']:
            k = event.waitKeys()
        if k[0] in ['q']:
            # quit task?
            response = -1
            print('Q pressed')
            cfg['ext']['quit'] = True
        if k[0] in ['left']:
            response = 0
            if strength > 0:
                correct = False
        if k[0] in ['right']:
            response = 1
            if strength == 0:
                correct = False


        # if correct:
        #     cfg['bin']['instruction'].text = 'correct'
        # else:
        #     cfg['bin']['instruction'].text = 'incorrect'

        cfg['bin']['instruction'].text = 'correct' if correct else 'incorrect'

        cfg['bin']['instruction'].draw()
        cfg['bin']['win'].flip()

        startTime = time.time()
        now = time.time()
        while now < (startTime + 1.0):
            cfg['bin']['instruction'].draw()
            cfg['bin']['win'].flip()
            now = time.time()

    return(cfg)

def getrunningStaircases(staircases):

    runningStaircases = []
    for i in range(len(staircases)):
        if staircases[i].running:
            runningStaircases.append(i)

    return(runningStaircases)

def runStaircases(cfg):

    runningStaircases = getrunningStaircases(cfg['bin']['staircases'])

    cfg['state']['currenttrial'] = 1
    cfg['ext']['quit'] = False

    while len(runningStaircases):
        

        if cfg['ext']['quit']:
            break
        # no non-stimulus trials?
        # runningStaircases += [-1] * int(np.ceil(len(runningStaircases)/2))


        random.shuffle(runningStaircases)
        for staircase_idx in runningStaircases:

            # build in a break here? once every 80 trials?
            if ((cfg['state']['currenttrial'] - 1) % 80) == 0:
                cfg = doBreak(cfg)
                # if q pressed, break
                if cfg['ext']['quit']:
                    break

            # take care of the actual trial
            cfg['state']['currentStaircase'] = staircase_idx
            cfg = runDetectionTrial(cfg)
            cfg['state']['currenttrial'] += 1

            # if q pressed, break
            if cfg['ext']['quit']:
                break

        # if q pressed, break
        if cfg['ext']['quit']:
            break

        runningStaircases = getrunningStaircases(cfg['bin']['staircases'])


    return(cfg)

def doBreak(cfg):

    # cfg['bin']['instruction'] = visual.TextStim(win=cfg['bin']['win'], text='hello world', pos=[0,0], colorSpace='rgb', color='#999999')
    cfg['bin']['instruction'].text = 'Press SPACE to start the next block.'
    cfg['bin']['instruction'].draw()
    cfg['bin']['win'].flip()

    # wait for press of the space bar:
    k = ['wait']
    while k[0] not in ['space', 'q']:
        k = event.waitKeys()
        # cfg['bin']['instruction'].draw()
        # cfg['bin']['win'].flip()

    if k[0] in ['q']:
        cfg['ext']['quit'] = True

    return(cfg)

def runDetectionTrial(cfg):

    staircase_idx = cfg['state']['currentStaircase']
    if staircase_idx < 0:
        # do a trial without a stimulus
        prepcommand = 'M2.S0.D0.'
        motor = 2
        strength = 0
        duration = 0

    else:
        # do a trial with a stimulus
        stepvalue = cfg['bin']['staircases'][staircase_idx].getValue()

        prepcommand = ''
        info = cfg['bin']['staircases'][staircase_idx].info
        if 'motor' in info.keys():
            motor = info['motor']
        else:
            motor = 2 # default to both motors?
        if 'strength' in info.keys():
            strength = info['strength'] # check valid value? (0-127)
        else:
            pass # is there a reasonable default?
        if 'duration' in info.keys():
            duration = info['duration']

        # assign these after defaults, such that staircase values are not overruled by defaults:
        if info['stepvalue'] == 'strength':
            strength = stepvalue
        if info['stepvalue'] == 'duration':
            duration = stepvalue
        
        prepcommand += 'M%d.S%d.D%d.'%(motor, strength, duration)

    cfg['bin']['TVFU'].write(bytes(prepcommand, 'utf-8'))

    # blank screen
    blank = 0.5
    # stimulus cue
    # wait interval
    # stimulus jitter
    jitter = random.uniform(0, 1)
    # stimulus presentation
    stimulusOnset = blank + jitter
    # response cue
    cueTime = blank + 1.5
    # get response
    # store data and end trial
    startTime = time.time()

    presentStimulus = True
    inStimulusInterval = True
    cfg['bin']['cursor'].fillColor = '#ff0000'


    while inStimulusInterval:
        now = time.time()
        if now > (startTime + stimulusOnset):
            # do tactile stimulation
            if (presentStimulus):
                presentStimulus = False
                cfg['bin']['TVFU'].write(bytes('G2.', 'utf-8'))

            pass
        if now > (startTime + cueTime):
            cfg['bin']['cursor'].fillColor = '#0000ff'
            inStimulusInterval = False
        if now > (startTime + blank):
            cfg['bin']['cursor'].draw()
        cfg['bin']['win'].flip()
    
    # wait for response
    k = ['wait']

    # left  = no stimulus detected  -> go up the staircase
    # right = detected stimulus     -> go down the staircase

    while k[0] not in ['left', 'right', 'q']:
        k = event.waitKeys()
    if k[0] in ['q']:
        # quit task?
        response = -1
        print('Q pressed')
        cfg['ext']['quit'] = True
    if k[0] in ['left']:
        response = 0
        if staircase_idx >= 0:
            cfg['bin']['staircases'][staircase_idx].update(+1)
    if k[0] in ['right']:
        response = 1
        if staircase_idx >= 0:
            cfg['bin']['staircases'][staircase_idx].update(-1)

    # cfg['state']['responses'] = {'trial'      = [],
    #                              'staircase'  = [],
    #                              'strength'   = [],
    #                              'duration'   = [],
    #                              'timejitter' = [],
    #                              'response'   = [],
    #                              }

    cfg['state']['responses']['trial'].append(cfg['state']['currenttrial'])
    cfg['state']['responses']['staircase'].append(staircase_idx)
    cfg['state']['responses']['strength'].append(strength)
    cfg['state']['responses']['duration'].append(duration)
    cfg['state']['responses']['timejitter'].append(jitter)
    cfg['state']['responses']['response'].append(response)

    pd.DataFrame(cfg['state']['responses']).to_csv('%s/staircase_data.csv'%(cfg['state']['dataFolder']), index=False)



    # store data in different location from movement task data


    return(cfg)

def runTrial(cfg):

    trialNumber =  cfg['state']['trialNumber']
    print(trialNumber)

    condIdx = cfg['state']['trialOrder'][trialNumber]
    targetSize = cfg['state']['conditions']['targetSize'][condIdx]
    tactileStim = cfg['state']['conditions']['tactileStim'][condIdx]
    targetPos = cfg['state']['conditions']['targetPos'][condIdx]
    print(targetPos)
    print("getting start pos")
    if trialNumber > 0:
        print("get startPos from previous trial")
        prevCondIdx = cfg['state']['trialOrder'][trialNumber-1]
        startPos = cfg['state']['conditions']['targetPos'][prevCondIdx]
    else:
        # startPos = [0, 0] 
        if tuple(targetPos) in cfg['state']['leftTargets']:

            # randomly pick a right Target

            startPos = random.sample(cfg['state']['rightTargets'], 1)[0]

        else:

            # randomly pick a left Target

            startPos = random.sample(cfg['state']['leftTargets'], 1)[0]
    print("assigning positions")
    cfg['bin']['target'].pos = targetPos
    cfg['bin']['target'].radius = targetSize
    cfg['bin']['start'].pos = startPos
    print("make rotation matrix")
    print(targetPos)
    print(startPos)
    theta = -1 * np.arctan2(targetPos[1] - startPos[1],targetPos[0] - startPos[0])
    R = np.array([[np.cos(theta),-1*np.sin(theta)],[np.sin(theta),np.cos(theta)]],order='C')

    runningTrial = True

    phase = 0

    # create lists to store data in:
    stylusX = []
    stylusY = []
    time_s = []
    phases = []

    print("start while loop")
    while runningTrial:

        [x,y,t] = cfg['bin']["tracker"].getPos()
        cursorPos = [x,y]
        cfg['bin']['cursor'].pos = cursorPos

        stylusX.append(x)
        stylusY.append(y)
        time_s.append(t)
        phases.append(phase)

        if phase == 3:
            runningTrial = False
        
        # after reaching the target, the participant needs to get to the next start position
        # (which MAY be smaller than the target)
        if phase == 2:
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            # check if hold-distance is maintained
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance > cfg['bin']['target'].radius:
                phase = 1
            # check if hold-time is complete
            if (time.time() > (holdStartTime + 0.1)):
                phase = 3

        # participant needs to move to the target
        # cursor and target are shown
        if phase == 1:
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            #check if target has been reached
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance < cfg['bin']['target'].radius:
                holdStartTime = time.time()
                phase = 2

        # at the start of the trial, participant needs to go to the start position
        # strat position and cursor are shown
        if phase == 0:
            cfg['bin']['start'].draw()
            cfg['bin']['cursor'].draw()
            # check if cursor is in the home position
            distance = ((cursorPos[0]-startPos[0])**2+(cursorPos[1]-startPos[1])**2)**0.5
            if distance < cfg['bin']['start'].radius:
                phase = 1

        # distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
        # if distance < 0.5:
        #     runningTrial = False
        # cfg['bin']['target'].draw()
        # cfg['bin']['start'].draw()
        # cfg['bin']['cursor'].draw()
        cfg['bin']['win'].flip()

        # add data sample for the current frame:

    
    # collect all data in matrix/dictionary?
    nsamples = len(time_s)

    trial_idx = [trialNumber+1] * nsamples
    targetx = [targetPos[0]] * nsamples
    targety = [targetPos[1]] * nsamples



    # put all lists in dictionary:
    trialdata = { 'trial_idx'         : trial_idx,
                  'targetx_cm'        : targetx,
                  'targety_cm'        : targety,
                  'phase'             : phases,      
                  'time_s'            : time_s,
                  'stylusx_cm'        : stylusX,
                  'stylusy_cm'        : stylusY,
                  }

    # make dictionary into data frame:
    trialdata = pd.DataFrame(trialdata)

    # store data frame:
    filename = 'data/%s/trial%04d.csv'%(cfg['state']['ID'],cfg['state']['trialNumber']+1)
    trialdata.to_csv( filename, index=False, float_format='%0.5f' )


    return(cfg)

def saveState(cfg):

    # generate a file name, open a stream, and save state as JSON into the stream
    
    filename = '%sstate.json'%(cfg["state"]["dataFolder"])

    with open(file=filename,
               mode='w') as fp:
        json.dump(cfg["state"], fp, indent=2)

    return(cfg)

def closeEnvironment(cfg):

    cfg['bin']['win'].close()

    return(cfg)


class SimpleStaircase:
    def __init__(self,steps,idx,minTrials,minReversals,info):
        self.steps = steps
        self.idx = idx
        self.minTrials = minTrials
        self.minReversals = minReversals
        self.info = info # arbitrary properties: put a dictionary or list for multiple bits of info

        self.running = True
        self.responses = []

    def getValue(self):
        return(self.steps[self.idx])

    def update(self, response):
        self.responses.append(response)
        self.idx += response
        if self.idx < 0:
            self.idx = 0
        if self.idx >= len(self.steps):
            self.idx = len(self.steps) - 1
        if len(self.responses) > 1:
            status = False
            if sum(np.diff(self.responses) != 0) < self.minReversals:
                status = True
            if len(self.responses) < self.minTrials:
                status = True
            self.running = status

class UDStaircase:
    def __init__(self,steps,idx,minTrials,minReversals,up,down,info):
        self.steps = steps
        self.idx = idx
        self.minTrials = minTrials
        self.minReversals = minReversals
        self.up = up
        self.down = down
        self.info = info # arbitrary properties: put a dictionary or list for multiple bits of info

        self.running = True
        self.responses = []

    def getValue(self):
        return(self.steps[self.idx])

    def update(self, response):
        self.responses.append(response)
        if response == 1:
            self.idx += self.up
        if response == -1:
            self.idx -= self.down
        
        if self.idx < 0:
            self.idx = 0
        if self.idx >= len(self.steps):
            self.idx = len(self.steps) - 1
        
        if len(self.responses) > 1:
            status = False
            if sum(np.diff(self.responses) != 0) < self.minReversals:
                status = True
            if len(self.responses) < self.minTrials:
                status = True
            self.running = status

class DescreasingStepStaircase:
    def __init__(self,steps,idx,minTrials,minReversals,step,decrease,info):
        self.steps = steps
        self.idx = idx
        self.minTrials = minTrials
        self.minReversals = minReversals
        self.step = step
        self.decrease = decrease
        self.info = info # arbitrary properties: put a dictionary or list for multiple bits of info

        self.running = True
        self.responses = []
        self.reversals = 0

    def getValue(self):
        return(self.steps[self.idx])

    def update(self, response):
        self.responses.append(response)
        if response == 1:
            self.idx += self.step
        if response == -1:
            self.idx -= self.step
        
        if self.idx < 0:
            self.idx = 0
        if self.idx >= len(self.steps):
            self.idx = len(self.steps) - 1
        
        if len(self.responses) > 1:
            status = False
            if sum(np.diff(self.responses) != 0) > self.reversals:
                self.reversals += 1
                self.step = max(1,self.step-self.decrease)
            if self.reversals < self.minReversals:
                status = True
            if len(self.responses) < self.minTrials:
                status = True
            self.running = status